use clap::Parser;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(long, short, help = "Program executed once, before processing begins")]
    start_program: String,

    #[clap(long, short, help = "Program executed per line of input")]
    line_program: String,

    #[clap(long, short, help = "Program executed once, after processing ends")]
    end_program: String,
    
    #[clap(long = "ifs", default_value = " ", help = "Input field separator")]
    input_field_separator: String,
    
    #[clap(long = "ofs", help = "Output field separator [default: same as --ifs]")]
    output_field_separator: Option<String>,

    #[clap(long, short, help = "Whether to consider consecutive separator strings as empty fields")]
    allow_empty_fields: bool
}

fn main() {
    let mut args = Args::parse();
    if args.output_field_separator.is_none() {
        args.output_field_separator = Some(args.input_field_separator);
    }
}
