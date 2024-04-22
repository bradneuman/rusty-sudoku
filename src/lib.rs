use std::cell::RefCell;
use std::collections::HashSet;
use std::error::Error;
use std::fmt;
use std::rc::Rc;

// TODO:(bn) consider making a "value" struct to hold only 1-9.

// TODO:(bn) then, probably change representation: just own a Constraint per cell, and then solve
// TODO:(bn) algorithm: iterate and mark as solved

// TODO:(bn) when stuck, then resolve box constraints by: iterate through each constraint in the box and
// determine if there is a unique column or row for a given number

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

// TODO:(bn) move to a util mod?
/// Sets the digit index val to the ascii decimal val. Panics if display is not big enough or val is out of
/// bounds.
fn set_char_to_digit(display: &mut Vec<char>, val: u8) {
    if let Some(digit) = char::from_digit(val as u32, 10) {
        display[(val - 1) as usize] = digit as char;
    } else {
        panic!("Values contained {val} which could not convert to char");
    }
}

impl fmt::Display for Puzzle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in &self.cells {
            for c in row {
                if let Some(v) = c.val {
                    let mut disp = vec!['.'; 9];
                    set_char_to_digit(&mut disp, v);
                    write!(f, "{} ", disp.into_iter().collect::<String>())?;
                } else {
                    write!(f, "{} ", c.constraint())?;
                }
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

    pub fn from_file(filename: &str) -> Result<Self, Box<dyn Error>> {
        let mut ret = Self::new();

        // TODO:(bn) import csv reader, read as csvs where empty is None
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
                    ret.solve_cell(r, c, v);
                }
            }
        }

        ret
    }

    fn solve_cell(&mut self, row: usize, col: usize, val: u8) {
        let cell = &mut self.cells[row][col];
        cell.val = Some(val);
        cell.row_c.borrow_mut().cant_be(val);
        cell.col_c.borrow_mut().cant_be(val);
        cell.box_c.borrow_mut().cant_be(val);
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
            set_char_to_digit(&mut disp, *v);
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
