use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;

// TODO:(bn) consider making a "value" struct to hold only 1-9.

#[derive(Debug)]
struct Cell {
    val: Option<u8>,
    row_c: Rc<RefCell<Constraint>>,
    col_c: Rc<RefCell<Constraint>>,
    box_c: Rc<RefCell<Constraint>>,
}

impl Cell {
    pub fn new(
        row_c: &Rc<RefCell<Constraint>>,
        col_c: &Rc<RefCell<Constraint>>,
        box_c: &Rc<RefCell<Constraint>>,
    ) -> Self {
        Self {
            val: None,
            row_c: Rc::clone(row_c),
            col_c: Rc::clone(col_c),
            box_c: Rc::clone(box_c),
        }
    }
}

impl Cell {
    pub fn constraint(&self) -> Constraint {
        let mut ret = Constraint::new();

        match self.val {
            Some(v) => ret.is(v),
            None => {
                ret.intersect(&*self.row_c.borrow());
                ret.intersect(&*self.col_c.borrow());
                ret.intersect(&*self.box_c.borrow());
            }
        }

        ret
    }
}

#[derive(Debug)]
pub struct Puzzle {
    cells: Vec<Vec<Cell>>,
}

impl fmt::Display for Puzzle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in &self.cells {
            for c in row {
                write!(f, "{} ", c.constraint())?;
            }
            write!(f, "\n")?;
        }
        fmt::Result::Ok(())
    }
}

#[derive(Debug)]
pub struct PartialPuzzle {
    pub cells: [[Option<u8>; 9]; 9],
}

impl PartialPuzzle {
    pub fn new() -> PartialPuzzle {
        PartialPuzzle {
            cells: [[None; 9]; 9],
        }
    }
}

impl fmt::Display for PartialPuzzle {
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

fn get_box(r: usize, c: usize) -> usize {
    ((r / 3) * 3 + (c / 3)) as usize
}

impl Puzzle {
    pub fn new(start: &PartialPuzzle) -> Puzzle {
        let mut ret = Puzzle::new_blank();

        for r in 0..9 {
            for c in 0..9 {
                if let Some(v) = start.cells[r][c] {
                    let cell = &mut ret.cells[r][c];
                    cell.val = Some(v);
                    cell.row_c.borrow_mut().cant_be(v);
                    cell.col_c.borrow_mut().cant_be(v);
                    cell.box_c.borrow_mut().cant_be(v);
                }
            }
        }

        ret
    }

    fn new_blank() -> Puzzle {
        let mut cells: Vec<Vec<Cell>> = Vec::new();

        let mut row_c: Vec<Rc<RefCell<Constraint>>> = Vec::new();
        let mut col_c: Vec<Rc<RefCell<Constraint>>> = Vec::new();
        let mut box_c: Vec<Rc<RefCell<Constraint>>> = Vec::new();

        for _ in 1..=9 {
            row_c.push(Rc::new(RefCell::new(Constraint::new())));
            col_c.push(Rc::new(RefCell::new(Constraint::new())));
            box_c.push(Rc::new(RefCell::new(Constraint::new())));
            cells.push(Vec::new());
        }

        for r in 0..9 {
            for c in 0..9 {
                cells[r].push(Cell::new(&row_c[r], &col_c[c], &box_c[get_box(r, c)]));
            }
        }

        Puzzle { cells }
    }
}

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

    /// Sets constraint to be solved (one possible value)
    pub fn is(&mut self, val: u8) {
        self.values.clear();
        self.values.insert(val);
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
