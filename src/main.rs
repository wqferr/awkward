use clap::Parser;

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
        requires_all(&["allow-empty-fields"]),
        help = "Whether to consider consecutive record separator strings as empty records"
    )]
    allow_empty_records: bool,

    #[clap(
        long,
        short,
        help = "Whether to skip the first record and interpret it as field names; TODO how can you reference these?"
    )]
    header: bool,

    #[clap(help = "Program executed once per record")]
    program: String,

    #[clap(help = "File to be processed; omit to use standard input")]
    input_file: Option<String>
}

fn main() {
    let mut args = Args::parse();
    if args.output_field_separator.is_none() {
        args.output_field_separator = Some(args.input_field_separator);
    }

    if args.output_record_separator.is_none() {
        args.output_record_separator = Some(args.input_record_separator);
    }
}
