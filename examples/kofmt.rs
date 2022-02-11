//! Print commands in a Dedukti file, one command per line.

use lambda_parse::term::Term;
use std::fs::File;
use std::io::{self, BufRead, BufReader};

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let filename = &args[1];

    let f = File::open(filename)?;
    let f = BufReader::new(f);
    for line in f.lines() {
        let line = line?;
        let term = Term::parse_str(&line).unwrap();
        //println!("{:?}", term);
    }

    Ok(())
}
