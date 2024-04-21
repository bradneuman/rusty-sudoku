use std::collections::HashSet;
use std::fmt;

#[derive(Debug)]
pub struct Constraint {
    // TODO: could store set as bits
    values: HashSet<u8>,
}

impl Constraint {
    /// Defaults to any value possible.
    pub fn new() -> Constraint {
        Constraint {
            values: (1..=9).collect(),
        }
    }
}

impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {        
        let mut v : Vec<_> = self.values.iter().collect();
        v.sort();
        let s : Vec<_> = v.iter().map(|x| x.to_string()).collect();
        write!(f, "{}", s.join(""))
    }
}
