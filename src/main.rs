use std::{
    ffi::OsString,
    fs::{self, File},
    io::{self, BufWriter, Read},
    path::Path,
    process::Command,
};

use anyhow::{anyhow, bail, Context, Result};
use ffmetadata::FFMetadata;
use serde::Deserialize;
use tempfile::{Builder, TempPath};

#[derive(Debug)]
pub struct Interval {
    pub start: u64,
    pub end: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SkipType {
    Opening,
    Ending,
    Recap,
    Unknown,
    Progress,
}

impl SkipType {
    pub fn format<F: core::fmt::Write>(&self, idx: usize, fmt: &mut F) -> core::fmt::Result {
        let name = match self {
            Self::Opening => "Opening",
            Self::Ending => "Ending",
            Self::Recap => "Recap",
            Self::Unknown => "Unknown",
            Self::Progress => "Progress",
        };
        if idx == 0 {
            write!(fmt, "{}", name)
        } else {
            write!(fmt, "{} {}", name, idx)
        }
    }
}

#[derive(Debug)]
pub struct Skip {
    pub interval: Interval,
    pub typ: SkipType,
}

impl Skip {
    pub fn to_kv(&self, idx: usize) -> (String, Vec<(String, String)>) {
        let mut kv = Vec::new();
        kv.push(("TIMEBASE".to_string(), "1/1000".to_string()));
        kv.push(("START".to_string(), self.interval.start.to_string()));
        kv.push(("END".to_string(), self.interval.end.to_string()));
        let mut title = String::new();
        _ = self.typ.format(idx, &mut title);
        title.shrink_to_fit();
        kv.push(("title".to_string(), title));

        ("CHAPTER".to_string(), kv)
    }
}

fn f32_u64(n: f32) -> u64 {
    (n as u64 * 1000) + (((n - (n as u64 as f32)) * 1000.0) as u64)
}

pub fn aniskip_deserialize<S: AsRef<str>>(s: S) -> impl Iterator<Item = Skip> {
    #[allow(non_snake_case)]
    #[derive(Debug, Deserialize)]
    struct Intervl {
        startTime: Option<f32>,
        endTime: Option<f32>,
    }
    #[allow(non_snake_case)]
    #[derive(Debug, Deserialize)]
    struct Item {
        interval: Option<Intervl>,
        skipType: Option<Box<str>>,
    }
    #[derive(Debug, Deserialize)]
    struct Results {
        results: Option<Vec<Item>>,
    }
    impl From<Item> for Option<Skip> {
        fn from(Item { interval, skipType }: Item) -> Self {
            let Intervl { startTime, endTime } = interval?;
            let interval = Interval {
                start: f32_u64(startTime?),
                end: f32_u64(endTime?),
            };
            let typ = match skipType.as_deref() {
                Some("op") | Some("mixed-op") => SkipType::Opening,
                Some("ed") | Some("mixed-ed") => SkipType::Ending,
                Some("recap") => SkipType::Recap,
                _ => SkipType::Unknown,
            };

            Some(Skip { interval, typ })
        }
    }

    let Results { results } = serde_json::from_str::<Results>(s.as_ref()).unwrap_or(Results {
        results: Some(Vec::new()),
    });
    let results = results.unwrap_or(Vec::new());

    results.into_iter().filter_map(Into::into)
}

fn eq_lowercase(s1: &str, s2: &str) -> bool {
    let it1 = s1.chars().flat_map(|c| c.to_lowercase());
    let it2 = s2.chars().flat_map(|c| c.to_lowercase());
    it1.eq(it2)
}

fn dump_metadata<P: AsRef<Path>>(file: P) -> Result<(FFMetadata, TempPath)> {
    let (mut f, p) = Builder::new()
        .suffix(".txt")
        .tempfile_in(file.as_ref().parent().unwrap())?
        .into_parts();

    let mut cmd = Command::new("ffmpeg");
    cmd.args(["-hide_banner", "-y", "-i"])
        .arg(file.as_ref())
        .args(["-f", "ffmetadata"])
        .arg(&p);
    println!("{cmd:?}");
    if !cmd.status()?.success() {
        return Err(io::Error::new(io::ErrorKind::Other, "ffmpeg exited with errors").into());
    }

    let mut buf = String::new();
    f.read_to_string(&mut buf)?;
    drop(f);
    let mut md = FFMetadata::parse(&buf).map_err(|e| anyhow!("{}", e))?;
    drop(buf);
    md.sections
        .retain(|(name, _)| !eq_lowercase(name, "chapter"));

    Ok((md, p))
}

fn generate_metadata_file<P: AsRef<Path>, S: AsRef<str>>(
    file: P,
    body: S,
    len: u64,
) -> Result<Option<TempPath>> {
    #[derive(Default)]
    struct Ids {
        opening: usize,
        ending: usize,
        recap: usize,
        unknown: usize,
        progress: usize,
    }
    impl Ids {
        pub fn get(&mut self, typ: SkipType) -> usize {
            let n = match typ {
                SkipType::Opening => &mut self.opening,
                SkipType::Ending => &mut self.ending,
                SkipType::Recap => &mut self.recap,
                SkipType::Unknown => &mut self.unknown,
                SkipType::Progress => &mut self.progress,
            };
            let res = *n;
            *n += 1;
            res
        }
    }

    let mut ids = Ids::default();
    let mut skips = aniskip_deserialize(body).collect::<Vec<_>>();
    skips.sort_unstable_by_key(|x| x.interval.start);
    let mut it = skips.into_iter();
    let mut prev_time = 0u64;
    let mut path = None;
    if let Some(skip) = it.next() {
        let (mut md, p) = dump_metadata(file)?;

        if skip.interval.start > prev_time {
            let s = Skip {
                interval: Interval {
                    start: prev_time,
                    end: skip.interval.start - 1,
                },
                typ: SkipType::Progress,
            };
            md.sections.push(s.to_kv(ids.get(s.typ)));
        }
        md.sections.push(skip.to_kv(ids.get(skip.typ)));
        prev_time = skip.interval.end;

        for skip in it {
            if skip.interval.start > prev_time {
                let s = Skip {
                    interval: Interval {
                        start: prev_time,
                        end: skip.interval.start - 1,
                    },
                    typ: SkipType::Progress,
                };
                md.sections.push(s.to_kv(ids.get(s.typ)));
            }
            md.sections.push(skip.to_kv(ids.get(skip.typ)));
            prev_time = skip.interval.end;
        }
        if prev_time < len {
            let s = Skip {
                interval: Interval {
                    start: prev_time + 1,
                    end: len,
                },
                typ: SkipType::Progress,
            };
            md.sections.push(s.to_kv(ids.get(s.typ)));
        }

        let f = File::create(&p)?;
        println!("{md}");
        path = Some(p);
        let mut writer = BufWriter::new(f);
        use std::io::Write;
        write!(writer, "{}", md)?;
    }
    Ok(path)
}

fn get_length<P: AsRef<Path>>(path: P) -> f32 {
    let mut cmd = Command::new("ffprobe");
    cmd.args([
        "-v",
        "error",
        "-show_entries",
        "format=duration",
        "-of",
        "default=noprint_wrappers=1:nokey=1",
    ]);
    cmd.arg(path.as_ref());
    cmd.output()
        .ok()
        .and_then(|o| {
            if o.status.success() {
                Some(o.stdout)
            } else {
                None
            }
        })
        .and_then(|o| {
            std::str::from_utf8(&o)
                .ok()
                .and_then(|o| o.trim().parse::<f32>().ok())
        })
        .unwrap_or(0.0)
}

fn ffmpeg<P: AsRef<Path>>(file: P, metadata_file: TempPath) -> io::Result<()> {
    let file = file.as_ref();

    let mut builder = Builder::new();
    let suffix = file.extension().map(|ext| {
        let mut suffix = OsString::new();
        suffix.push(".");
        suffix.push(ext);
        suffix
    });
    if let Some(suffix) = suffix.as_ref() {
        builder.suffix(suffix);
    }
    let (_, tmpfile) = builder
        .make_in(file.parent().unwrap(), |path| File::create(path).map(drop))?
        .into_parts();

    let mut cmd = Command::new("ffmpeg");
    cmd.args(["-hide_banner", "-y", "-i"])
        .arg(&metadata_file)
        .arg("-i")
        .arg(file)
        .args([
            "-map_metadata",
            "0",
            "-map",
            "1",
            "-c:v",
            "copy",
            "-c:a",
            "copy",
            "-c:s",
            "copy",
        ])
        .arg(&tmpfile);
    println!("{cmd:?}");
    if !cmd.status()?.success() {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "ffmpeg exited with errors",
        ));
    }

    tmpfile.persist(file)?;
    Ok(())
}

fn usage() {
    let name = std::env::args().next();
    let name = if let Some(n) = name.as_deref() {
        n
    } else {
        "ffmpeg-auskip"
    };
    eprintln!("USAGE: {} <file> <myanimelist_id> <episode_number>", name);
}

fn _main() -> Result<()> {
    let (file, mal_id, epno) = if std::env::args().len() == 4 {
        let mut it = std::env::args();
        it.next();
        let file = it.next().unwrap();
        let mal_id = it.next().unwrap();
        let epno = it.next().unwrap();

        let file = fs::canonicalize(file).context("file does not exist")?;
        if !file.is_file() {
            bail!("path is not a file");
        }
        let mal_id = mal_id.parse::<u64>().context("invalid myanimelist_id")?;
        let epno = epno.parse::<u64>().context("invalid episode_number")?;

        (file, mal_id, epno)
    } else {
        usage();
        std::process::exit(1);
    };

    let len = get_length(&file);
    let url = format!("https://api.aniskip.com/v2/skip-times/{}/{}?types=op&types=ed&types=mixed-op&types=mixed-ed&types=recap&episodeLength=0", mal_id, epno);
    let body = ureq::get(&url)
        .call()
        .and_then(|res| Ok(res.into_string()?))
        .context("error in request")?;
    drop(url);
    let Some(metadata_file) =
        generate_metadata_file(&file, body, f32_u64(len)).context("Cannot create metadata file")?
    else {
        return Ok(());
    };

    ffmpeg(file, metadata_file).context("Error while writing metadata into video")?;
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    _main()?;
    Ok(())
}
