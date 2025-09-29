use std::{
    ffi::OsString,
    fs::File,
    io::{self, BufWriter, Read},
    path::{Path, PathBuf},
    process::Command,
    str::FromStr,
};

use anyhow::{anyhow, Context, Result};
use ffmetadata::FFMetadata;
use getopt::Opt;
use serde::Deserialize;
use tempfile::{Builder, TempPath};

#[derive(Debug)]
struct CliArgs {
    pub anilist_id: Option<u64>,
    pub myanimelist_id: Option<u64>,
    pub track: Option<u64>,
    pub file: Box<Path>,
}

#[derive(Debug)]
pub enum DOption<T> {
    Default(T),
    Some(T),
    None,
}

impl<T: Clone> Clone for DOption<T> {
    fn clone(&self) -> Self {
        match self {
            Self::Default(v) => Self::Default(v.clone()),
            Self::Some(v) => Self::Some(v.clone()),
            Self::None => Self::None,
        }
    }
}

impl<T: std::marker::Copy> std::marker::Copy for DOption<T> {}

impl<T> DOption<T> {
    #[inline(always)]
    pub fn is_some(&self) -> bool {
        matches!(self, DOption::Some(_))
    }

    #[inline(always)]
    pub fn is_none(&self) -> bool {
        matches!(self, DOption::None)
    }

    #[inline(always)]
    pub fn into_option(self) -> Option<T> {
        self.into()
    }
}

impl<T> From<DOption<T>> for Option<T> {
    fn from(value: DOption<T>) -> Self {
        match value {
            DOption::Default(v) => Some(v),
            DOption::Some(v) => Some(v),
            DOption::None => None,
        }
    }
}

impl<T> From<Option<T>> for DOption<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(v) => Self::Some(v),
            None => Self::None,
        }
    }
}

fn chapter(interval: &Interval, title: String) -> (String, Vec<(String, String)>) {
    (
        "CHAPTER".to_string(),
        vec![
            ("TIMEBASE".to_string(), "1/1000".to_string()),
            ("START".to_string(), interval.start.to_string()),
            ("END".to_string(), interval.end.to_string()),
            ("title".to_string(), title),
        ],
    )
}

#[derive(Debug)]
struct Ctx {
    pub anilist_id: DOption<u64>,
    pub myanimelist_id: DOption<u64>,
    pub track: DOption<u64>,
    pub file: Box<Path>,
    pub metadata: FFMetadata,
    pub metadata_file: TempPath,
    pub new_skips: bool,
}

impl Ctx {
    pub fn needs_update(&self) -> bool {
        self.new_skips
            || self.anilist_id.is_some()
            || self.myanimelist_id.is_some()
            || self.track.is_some()
    }

    pub fn push_skips(&mut self, skips: Vec<Skip>) {
        #[derive(Default)]
        struct Ids {
            opening: usize,
            ending: usize,
            recap: usize,
            other: usize,
        }
        impl Ids {
            pub fn get(&mut self, typ: SkipType) -> usize {
                let n = match typ {
                    SkipType::Opening => &mut self.opening,
                    SkipType::Ending => &mut self.ending,
                    SkipType::Recap => &mut self.recap,
                };
                let res = *n;
                *n += 1;
                res
            }

            pub fn get_default(&mut self) -> usize {
                self.other += 1;
                self.other
            }
        }

        let mut ids = Ids::default();
        let mut it = skips.into_iter();
        if let Some(skip) = it.next() {
            let len = f32_to_u64(get_length(&self.file));
            self.metadata
                .sections
                .retain(|(name, _)| !eq_lowercase(name, "chapter"));
            if skip.interval.start > 0 {
                self.metadata.sections.push(chapter(
                    &Interval {
                        start: 0,
                        end: skip.interval.start,
                    },
                    format!("Chapter {}", ids.get_default()),
                ));
            }
            self.metadata.sections.push(skip.to_kv(ids.get(skip.typ)));
            let mut last = skip.interval.end;

            for skip in it {
                if skip.interval.start > last {
                    self.metadata.sections.push(chapter(
                        &Interval {
                            start: last,
                            end: skip.interval.start,
                        },
                        format!("Chapter {}", ids.get_default()),
                    ));
                }
                self.metadata.sections.push(skip.to_kv(ids.get(skip.typ)));
                last = skip.interval.end;
            }
            if len > last {
                self.metadata.sections.push(chapter(
                    &Interval {
                        start: last,
                        end: len,
                    },
                    format!("Chapter {}", ids.get_default()),
                ));
            }
            self.new_skips = true;
        }
    }

    pub fn materialize_metadata(&self) -> Result<()> {
        if self.needs_update() {
            let f = File::create(&self.metadata_file)?;
            let mut writer = BufWriter::new(f);
            use std::io::Write;
            write!(writer, "{}", self.metadata)?;
        }
        Ok(())
    }
}

impl TryFrom<CliArgs> for Ctx {
    type Error = anyhow::Error;

    fn try_from(
        CliArgs {
            anilist_id: cli_anilist_id,
            myanimelist_id: cli_myanimelist_id,
            track: cli_track,
            file,
        }: CliArgs,
    ) -> Result<Self> {
        let (mut f, p) = Builder::new()
            .suffix(".txt")
            .tempfile_in(file.parent().unwrap())?
            .into_parts();

        let mut cmd = Command::new("ffmpeg");
        cmd.args(["-hide_banner", "-y", "-i"])
            .arg({
                let file = std::fs::canonicalize(file.as_ref())?;
                let mut uri_path = OsString::from("file://");
                uri_path.push(file);
                uri_path
            })
            .args(["-f", "ffmetadata"])
            .arg(&p);
        if !cmd.status()?.success() {
            return Err(io::Error::other("ffmpeg exited with errors").into());
        }

        let mut buf = String::new();
        f.read_to_string(&mut buf)?;
        drop(f);
        let mut md = FFMetadata::parse(&buf).map_err(|e| anyhow!("{}", e))?;
        drop(buf);

        fn filtermd<T>(v: &str, res: &mut DOption<T>) -> bool
        where
            T: FromStr + PartialEq + Eq,
        {
            if let Ok(v) = v.parse::<T>() {
                if match res {
                    DOption::Some(r) => v == *r,
                    DOption::Default(_) => true,
                    DOption::None => true,
                } {
                    *res = DOption::Default(v);
                    true
                } else {
                    false
                }
            } else {
                !res.is_some()
            }
        }

        let mut anilist_id = cli_anilist_id.into();
        let mut myanimelist_id = cli_myanimelist_id.into();
        let mut track = cli_track.into();
        md.global.retain(|(name, value)| match name.as_str() {
            "anilist_id" => filtermd(value.as_str(), &mut anilist_id),
            "myanimelist_id" => filtermd(value, &mut myanimelist_id),
            "track" => filtermd(value, &mut track),
            _ => true,
        });
        if let DOption::Some(anilist_id) = anilist_id {
            md.global
                .push(("anilist_id".into(), anilist_id.to_string()))
        }
        if let DOption::Some(myanimelist_id) = myanimelist_id {
            md.global
                .push(("myanimelist_id".into(), myanimelist_id.to_string()))
        }
        if let DOption::Some(track) = track {
            md.global.push(("track".into(), track.to_string()))
        }

        Ok(Self {
            anilist_id,
            myanimelist_id,
            track,
            file,
            metadata: md,
            metadata_file: p,
            new_skips: false,
        })
    }
}

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
}

impl SkipType {
    pub fn format<F: core::fmt::Write>(&self, idx: usize, fmt: &mut F) -> core::fmt::Result {
        let name = match self {
            Self::Opening => "Opening",
            Self::Ending => "Ending",
            Self::Recap => "Recap",
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
        let mut title = String::new();
        _ = self.typ.format(idx, &mut title);
        title.shrink_to_fit();
        chapter(&self.interval, title)
    }
}

fn f32_to_u64(n: f32) -> u64 {
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
                start: f32_to_u64(startTime?),
                end: f32_to_u64(endTime?),
            };
            let typ = match skipType.as_deref() {
                Some("op") | Some("mixed-op") => SkipType::Opening,
                Some("ed") | Some("mixed-ed") => SkipType::Ending,
                Some("recap") => SkipType::Recap,
                _ => return None,
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
        .arg({
            let file = std::fs::canonicalize(&metadata_file)?;
            let mut uri_path = OsString::from("file://");
            uri_path.push(file);
            uri_path
        })
        .arg("-i")
        .arg({
            let file = std::fs::canonicalize(file)?;
            let mut uri_path = OsString::from("file://");
            uri_path.push(file);
            uri_path
        })
        .args([
            "-movflags",
            "+use_metadata_tags",
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
    if !cmd.status()?.success() {
        return Err(io::Error::other("ffmpeg exited with errors"));
    }

    tmpfile.persist(file)?;
    Ok(())
}

fn usage() {
    let name = std::env::args().next();
    let name = name.as_deref().unwrap_or("ffmpeg-auskip");
    eprintln!(
        "USAGE: {} [-a [?]anilist_id] [-m [?]myanimelist_id] [-t [?]episode_number] <file>",
        name
    );
}

macro_rules! usage {
    () => {{
        usage();
        std::process::exit(1);
    }};
    ($($tt:tt)+) => {{
        eprintln!($($tt)+);
        usage!();
    }}
}

fn args() -> Result<CliArgs> {
    let mut args: Vec<String> = std::env::args().collect();
    let mut opts = getopt::Parser::new(&args, "a:m:t:");
    let mut anilist_id = None;
    let mut myanimelist_id = None;
    let mut track = None;

    fn parse<T: FromStr>(s: &str, name: &str) -> Option<T> {
        if let Some(s) = s.strip_prefix('?') {
            s.parse::<T>().ok()
        } else {
            match s.parse::<T>() {
                Ok(v) => Some(v),
                Err(_) => {
                    usage!("Invalid {}", name);
                }
            }
        }
    }

    for opt in opts.by_ref() {
        match opt? {
            Opt('a', Some(string)) => anilist_id = parse(&string, "anilist_id"),
            Opt('m', Some(string)) => myanimelist_id = parse(&string, "myanimelist_id"),
            Opt('t', Some(string)) => track = parse(&string, "episode_number"),
            _ => unreachable!(),
        }
    }

    let mut args = args.split_off(opts.index());
    if args.is_empty() {
        usage!("No file given.")
    } else if args.len() > 1 {
        usage!("Too many files given.")
    }

    Ok(CliArgs {
        anilist_id,
        myanimelist_id,
        track,
        file: PathBuf::from(args.remove(0)).into(),
    })
}

#[inline(always)]
fn aniskip_url(mal_id: u64, track: u64) -> String {
    format!("https://api.aniskip.com/v2/skip-times/{}/{}?types=op&types=ed&types=mixed-op&types=mixed-ed&types=recap&episodeLength=0", mal_id, track)
}

fn _main() -> Result<()> {
    let mut ctx = Ctx::try_from(args()?)?;
    let skips = if let Some(url) = ctx
        .myanimelist_id
        .into_option()
        .and_then(|mal| ctx.track.into_option().map(|track| aniskip_url(mal, track)))
    {
        match ureq::get(&url)
            .call()
            .and_then(|res| Ok(res.into_string()?))
        {
            Ok(body) => {
                let mut skips = aniskip_deserialize(body).collect::<Vec<_>>();
                skips.sort_unstable_by_key(|x| x.interval.start);
                skips
            }
            Err(err) => {
                eprintln!("Error: {}", err);
                Vec::new()
            }
        }
    } else {
        Vec::new()
    };
    ctx.push_skips(skips);
    ctx.materialize_metadata()?;

    if ctx.needs_update() {
        ffmpeg(ctx.file, ctx.metadata_file).context("Error while writing metadata into video")?;
    } else {
        eprintln!("We do not need update?");
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    _main()?;
    Ok(())
}
