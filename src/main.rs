mod record;
use std::fs::File;
use std::fs::read_to_string;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Read;
use std::io::Write;
use std::io::stdin;
use std::io::stdout;

use clap::Parser;
use clap::ValueHint;

use anyhow;
use record::Record;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(long, short, help = "Program executed once, before processing begins")]
    start_program: Option<String>,

    #[clap(long, short, help = "Program executed once, after processing ends")]
    end_program: Option<String>,
    
    #[clap(long = "ifs", env = "IFS", default_value = " ", help = "Input field separator")]
    input_field_separator: String,
    
    #[clap(long = "ofs", env = "OFS", help = "Output field separator")]
    output_field_separator: Option<String>,

    // read until newline, this is too much of a headache
    // #[clap(long = "irs", env = "IRS", default_value_t = '\n', help = "Input record separator")]
    // input_record_separator: char,

    #[clap(long = "ors", env = "ORS", default_value = "\n", help = "Output record separator")]
    output_record_separator: String,

    #[clap(long, short, help = "Whether to consider consecutive field separator strings as empty fields")]
    allow_empty_fields: bool,

    #[clap(
        long,
        short = 'A',
        requires("allow-empty-fields"),
        help = "Whether to consider consecutive record separator strings as empty records"
    )]
    allow_empty_records: bool,

    #[clap(
        long,
        short,
        help = "Whether to trim whitespace off the beginning and end of fields"
    )]
    dont_trim_fields: bool,

    #[clap(
        long,
        short = 'H',
        help = "Whether to skip the first record and interpret it as field names; TODO how can you reference these?"
    )]
    header: bool,

    #[clap(
        help = "Program executed once per record",
        required_unless_present = "program-file",
        group = "main_program"
    )]
    program: Option<String>,

    #[clap(
        long,
        short = 'f',
        group = "main_program",
        value_hint = ValueHint::FilePath,
        help = "File containing program executed once per record; if given, <PROGRAM> is no longer required",
    )]
    program_file: Option<String>,

    #[clap(
        value_hint = ValueHint::FilePath,
        help = "File to be processed; omit to use standard input",
    )]
    input_file: Option<String>
}

fn main() -> anyhow::Result<()> {
    let mut args = Args::parse();
    // let mut ifs, ors, ofs;

    let ifs = args.input_field_separator;
    let ofs = args.output_field_separator.unwrap_or_else(|| ifs.clone());
    let ors = args.output_record_separator;

    // if args.output_field_separator.is_none() {
    //     ofs = Some(args.input_field_separator.clone());
    // }

    // if args.output_record_separator.is_none() {
    //     args.output_record_separator = Some(args.input_record_separator.clone());
    // }

    if args.program_file.is_some() {
        let path = args.program_file.unwrap();
        args.program = Some(read_to_string(path)?);
    }

    let sin = stdin();
    let sout = stdout();

    let mut input: Box<dyn BufRead>;

    if let Some(path) = args.input_file {
        input = Box::new(BufReader::new(File::open(path)?));
    } else {
        input = Box::new(sin.lock());
    }

    let mut lock = sout.lock();
    loop {
        // both flags are inverted, but their names make more sense like this
        let record = Record::read(&mut input, &ifs)?
            .process(!args.allow_empty_fields, !args.dont_trim_fields);
        record.write(&mut lock, &ofs)?;
        lock.write(ors.as_bytes())?;

        let buf = input.fill_buf()?;
        if buf.is_empty() {
            break;
        }
    }
    Ok(())
}