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

    /// Removes `val` from the set of values that can be represented, if present (otherwise, does nothing).
    pub fn cant_be(&mut self, val: u8) {
        self.values.remove(&val);
    }

    /// Updates this constraint to become the intersection of this and rhs
    pub fn intersect(&mut self, rhs: &Constraint) {
        self.values = self.values.intersection(&rhs.values).cloned().collect();
    }

    /// The constraint is considered "solved" if there is exactly one value possible. If so, returns that
    /// value, otherwise None.
    pub fn solution(&self) -> Option<u8> {
        // TODO:(bn) more idiomatic way to do this? Trying to avoid calling len...
        let mut it = self.values.iter();
        if let Some(v) = it.next() {
            if it.next() == None {
                return Some(*v);
            }
        }
        None
    }
}

impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut disp = vec!['_'; 9];
        for v in self.values.iter() {
            if let Some(c) = char::from_digit(*v as u32, 10) {
                disp[(*v - 1) as usize] = c as char;
            } else {
                panic!("Values contained {v} which could not convert to char");
            }
        }
        write!(f, "{}", disp.into_iter().collect::<String>())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new() {
        let c = Constraint::new();
        assert_eq!(c.to_string(), "123456789");
    }

    #[test]
    fn test_cant_be() {
        let mut c = Constraint::new();

        c.cant_be(2);
        c.cant_be(4);
        c.cant_be(9);
        assert_eq!(c.to_string(), "1_3_5678_");
    }

    #[test]
    fn test_intersect() {
        let mut c = Constraint::new();
        c.cant_be(2);
        c.cant_be(4);
        c.cant_be(9);

        let mut b = Constraint::new();
        b.cant_be(5);

        c.intersect(&b);
        assert_eq!(c.to_string(), "1_3__678_");
    }

    #[test]
    fn test_no_solution_on_new() {
        let c = Constraint::new();
        assert_eq!(c.solution(), None);
    }

    #[test]
    fn test_no_solution_on_few() {
        let mut c = Constraint::new();
        c.cant_be(1);
        c.cant_be(2);
        c.cant_be(3);
        c.cant_be(4);
        c.cant_be(5);
        c.cant_be(9);
        assert_eq!(c.solution(), None);
    }

    #[test]
    fn test_solution() {
        let mut c = Constraint::new();
        c.cant_be(1);
        c.cant_be(2);
        c.cant_be(3);
        c.cant_be(4);
        c.cant_be(5);
        c.cant_be(6);
        // 7 should be the solution
        c.cant_be(8);
        c.cant_be(9);
        assert_eq!(c.solution(), Some(7));
    }
}
