use std::fmt;

/// Sets the digit index val to the ascii decimal val. Panics if display is not big enough or val is out of
/// bounds.
pub fn set_char_to_digit(display: &mut Vec<char>, val: u8) {
    if let Some(digit) = char::from_digit(val as u32, 10) {
        display[(val - 1) as usize] = digit as char;
    } else {
        panic!("Values contained {val} which could not convert to char");
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Coords {
    pub row: usize,
    pub col: usize,
}

impl Coords {
    pub fn from(row: usize, col: usize) -> Self {
        Self { row, col }
    }
}

impl fmt::Display for Coords {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.row, self.col)
    }
}


// TODO:(bn) tests!
