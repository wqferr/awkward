mod record;
use std::fs::read_to_string;
use std::io::BufRead;
use std::io::Write;
use std::io::stdin;
use std::io::stdout;

use clap::Parser;
use clap::ValueHint;

use program::Program;
use record::Record;

mod grammar;
mod types;
mod expr;
mod program;

#[cfg(windows)]
const DEFAULT_LINE_ENDING: &str = "\r\n";
#[cfg(not(windows))]
const DEFAULT_LINE_ENDING: &str = "\n";

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    // #[clap(long, short, help = "Program executed once, before processing begins")]
    // start_program: Option<String>,

    // #[clap(long, short, help = "Program executed once, after processing ends")]
    // end_program: Option<String>,
    
    #[clap(short = 'F', long = "ifs", env = "IFS", default_value = " ", help = "Input field separator")]
    input_field_separator: String,
    
    #[clap(long = "ofs", env = "OFS", help = "Output field separator; defaults to ifs")]
    output_field_separator: Option<String>,

    // read until newline, this is too much of a headache
    // #[clap(long = "irs", env = "IRS", default_value_t = '\n', help = "Input record separator")]
    // input_record_separator: char,

    #[clap(long = "ors", env = "ORS", default_value = DEFAULT_LINE_ENDING, help = "Output record separator")]
    output_record_separator: String,

    #[clap(short = 'e', long, help = "Whether to consider consecutive field separator strings as empty fields")]
    allow_empty_fields: bool,

    #[clap(
        long,
        short = 'E',
        requires("allow-empty-fields"),
        help = "Whether to consider consecutive record separator strings as empty records"
    )]
    allow_empty_records: bool,

    #[clap(
        short = 'd',
        long,
        help = "Whether to trim whitespace off the beginning and end of fields"
    )]
    dont_trim_fields: bool,

    #[clap(
        short = 'H',
        long,
        help = "Whether to skip the first record and interpret it as field names"
    )]
    header: bool,

    #[clap(
        short = 'v',
        long = "assign",
        help = "Assign the value val to the variable var; usage: -v var=val; this option can be repeated",
    )]
    vars: Option<Vec<String>>,

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
}

fn main() -> anyhow::Result<()> {
    let mut args = Args::parse();

    let ifs = args.input_field_separator;

    // avoid ifs.clone() if ofs is defined
    let ofs = args.output_field_separator.unwrap_or_else(|| ifs.clone());
    let ors = args.output_record_separator;

    if args.program_file.is_some() {
        let path = args.program_file.unwrap();
        args.program = Some(read_to_string(path)?);
    }

    let mut prog = Program::new(ofs, ors);
    prog.push_rules(Program::compile(args.program.unwrap().as_str()));

    let sin = stdin();
    let sout = stdout();

    let mut input = sin.lock();
    let mut output = sout.lock();

    if args.header {
        let mut field_names = Record::read(&mut input, &ifs)?;
        field_names.process(false, true);
        prog.set_field_names(field_names.clone().into_iter().collect());
        prog.start(Some(field_names));
    } else {
        prog.start(None);
    }
    loop {
        let mut record = Record::read(&mut input, &ifs)?;

        // both flags are inverted here, but their names make more sense like this
        record.process(!args.allow_empty_fields, !args.dont_trim_fields);

        prog.consume(record);
        output.write_all(prog.last_output().as_bytes())?;

        let buf = input.fill_buf()?;
        if buf.is_empty() {
            break;
        }
    }
    prog.end();
    // TODO check for output produced in end
    Ok(())
}