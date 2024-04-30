use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct Puzzle {
    pub cells: [[Option<u8>; 9]; 9],
}

impl Puzzle {
    pub fn new() -> Puzzle {
        Puzzle {
            cells: [[None; 9]; 9],
        }
    }

    pub fn from_file(filename: &str) -> Result<Self, Box<dyn Error>> {
        let mut ret = Self::new();

        let mut rdr = csv::ReaderBuilder::new()
            .has_headers(false)
            .from_path(filename)?;

        for (r, result) in rdr.records().enumerate() {
            let row = result?;
            for (c, val) in row.iter().enumerate() {
                ret.cells[r][c] = match val.parse::<u8>() {
                    Ok(v @ 1..=9) => Some(v),
                    _ => None,
                }
            }
        }

        Ok(ret)
    }
}

impl fmt::Display for Puzzle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let hl = "----------------------";

        for r in 0..9 {
            if r % 3 == 0 {
                write!(f, "{}\n", hl)?;
            }
            write!(f, "|")?;
            for c in 0..9 {
                write!(
                    f,
                    "{} ",
                    match self.cells[r][c] {
                        None => '_',
                        Some(v) => char::from_digit(v as u32, 10).unwrap(),
                    }
                )?;
                if c % 3 == 2 {
                    write!(f, "|")?;
                }
            }
            write!(f, "\n")?;
        }
        write!(f, "{}\n", hl)
    }
}
