use std::fs::read_to_string;

use clap::Parser;
use clap::ValueHint;

use anyhow;

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

    #[clap(long = "irs", env = "IRS", default_value = "\n", help = "Input record separator")]
    input_record_separator: String,

    #[clap(long = "ors", env = "ORS", help = "Output record separator")]
    output_record_separator: Option<String>,

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

    #[clap(help = "File to be processed; omit to use standard input")]
    input_file: Option<String>
}

fn main() -> anyhow::Result<()> {
    let mut args = Args::parse();
    if args.output_field_separator.is_none() {
        args.output_field_separator = Some(args.input_field_separator);
    }

    if args.output_record_separator.is_none() {
        args.output_record_separator = Some(args.input_record_separator);
    }

    if args.program_file.is_some() {
        let path = args.program_file.unwrap();
        args.program = Some(read_to_string(path)?);
    }
    Ok(())
}