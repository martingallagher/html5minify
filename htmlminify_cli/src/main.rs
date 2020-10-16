use std::fs::File;
use std::io;
use std::path::PathBuf;

use html5minify::Minifier;
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(name = "html5minify", about = "HTML5Minify options")]
struct Opt {
    /// Preserve whitespace
    #[structopt(short = "w", long = "whitespace")]
    disable_collapse_whitespace: bool,

    /// Omit HTML5 doctype
    #[structopt(short = "d", long = "doctype")]
    omit_doctype: bool,

    /// Preserve HTML comments
    #[structopt(short = "c", long = "comments")]
    preserve_comments: bool,

    /// Input file
    #[structopt(parse(from_os_str))]
    input: PathBuf,

    /// Output file, stdout if not set
    #[structopt(parse(from_os_str))]
    output: Option<PathBuf>,
}

fn main() -> io::Result<()> {
    let opt = Opt::from_args();
    let mut input = File::open(&opt.input)?;

    if let Some(output) = &opt.output {
        Minifier::new(&mut File::create(&output)?)
            .collapse_whitespace(!opt.disable_collapse_whitespace)
            .omit_doctype(opt.omit_doctype)
            .preserve_comments(opt.preserve_comments)
            .minify(&mut input)
    } else {
        Minifier::new(&mut io::stdout())
            .collapse_whitespace(!opt.disable_collapse_whitespace)
            .omit_doctype(opt.omit_doctype)
            .preserve_comments(opt.preserve_comments)
            .minify(&mut input)
    }
}
