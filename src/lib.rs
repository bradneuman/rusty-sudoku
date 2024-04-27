use std::collections::HashMap;
use std::collections::HashSet;
use std::error::Error;
use std::fmt;

// TODO:(bn) consider making a "value" struct to hold only 1-9.

// TODO:(bn) algorithm: iterate and mark as solved

// TODO:(bn) when stuck, then resolve box constraints by: iterate through each constraint in the box and
// determine if there is a unique column or row for a given number

#[derive(Debug)]
pub struct Cell {
    solution: Option<u8>,
    constraint: Constraint,
}

impl Cell {
    pub fn new() -> Self {
        Self {
            solution: None,
            constraint: Constraint::new(),
        }
    }

    pub fn cant_be(&mut self, val: u8) -> bool {
        self.constraint.cant_be(val)
    }
}

impl Cell {
    pub fn constraint(&self) -> &Constraint {
        &self.constraint
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
        for (r_index, row) in self.cells.iter().enumerate() {
            for (c_index, c) in row.iter().enumerate() {
                if let Some(v) = c.solution {
                    let mut disp = vec!['.'; 9];
                    set_char_to_digit(&mut disp, v);
                    write!(f, "{} ", disp.into_iter().collect::<String>())?;
                } else {
                    write!(f, "{} ", c.constraint())?;
                }

                if c_index % 3 == 2 {
                    write!(f, "  ")?;
                }
            }
            write!(f, "\n")?;
            if r_index % 3 == 2 {
                write!(f, "\n")?;
            }
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

/// get hte box index that contains (r,c)
fn get_box_index(r: usize, c: usize) -> usize {
    ((r / 3) * 3 + (c / 3)) as usize
}

/// Return the top-left index of the box containing (r,c)
fn get_box_start(index: usize) -> (usize, usize) {
    let row = (index / 3) * 3;
    let col = (index % 3) * 3;
    (row, col)
}
// TODO:(bn) tese these utils!!

#[derive(Debug, PartialEq)]
pub enum PuzzleIterType {
    Row,
    Col,
    Box,
}

impl Puzzle {
    pub fn new(start: &PartialPuzzle) -> Puzzle {
        let mut ret = Puzzle::new_blank();

        for r in 0..9 {
            for c in 0..9 {
                if let Some(v) = start.cells[r][c] {
                    if v > 0 && v <= 9 {
                        ret.solve_cell(r, c, v);
                    }
                }
            }
        }

        ret
    }

    fn solve_cell(&mut self, row: usize, col: usize, val: u8) {
        let cell = &mut self.cells[row][col];
        if let Some(old_val) = cell.solution {
            if old_val != val {
                panic!("Solver error: cell ({row}, {col}) already had solution {old_val}, trying to overwrite with {val}");
            }
        }

        cell.solution = Some(val);

        self.for_all_sets(row, col, &mut |_, _, cell: &mut Cell| {
            cell.cant_be(val);
        });
    }

    fn new_blank() -> Puzzle {
        let mut cells: Vec<Vec<Cell>> = Vec::new();

        for r in 0..9 {
            cells.push(Vec::new());
            for _ in 0..9 {
                cells[r].push(Cell::new());
            }
        }

        Puzzle { cells }
    }

    fn for_row<F>(&mut self, row: usize, f: &mut F)
    where
        F: FnMut(usize, usize, &mut Cell) -> (),
    {
        for c in 0..9 {
            f(row, c, &mut self.cells[row][c]);
        }
    }

    fn for_col<F>(&mut self, col: usize, f: &mut F)
    where
        F: FnMut(usize, usize, &mut Cell) -> (),
    {
        for r in 0..9 {
            f(r, col, &mut self.cells[r][col]);
        }
    }

    /// Call F for each item in the 3x3 box around (row,col)
    fn for_box_containing<F>(&mut self, row: usize, col: usize, f: &mut F)
    where
        F: FnMut(usize, usize, &mut Cell) -> (),
    {
        self.for_box_index(get_box_index(row, col), f);
    }

    /// Call F for each item in the box indexed 0..9
    fn for_box_index<F>(&mut self, index: usize, f: &mut F)
    where
        F: FnMut(usize, usize, &mut Cell) -> (),
    {
        let (box_r, box_c) = get_box_start(index);
        for r in box_r..(box_r + 3) {
            for c in box_c..(box_c + 3) {
                f(r, c, &mut self.cells[r][c]);
            }
        }
    }

    /// Iterates over the row, column, then box associated with the given cell.
    fn for_all_sets<F>(&mut self, row: usize, col: usize, f: &mut F)
    where
        F: FnMut(usize, usize, &mut Cell) -> (),
    {
        self.for_row(row, f);
        self.for_col(col, f);
        self.for_box_containing(row, col, f);
    }

    fn for_all<F>(&mut self, iter_type: &PuzzleIterType, index: usize, f: &mut F)
    where
        F: FnMut(usize, usize, &mut Cell) -> (),
    {
        match iter_type {
            PuzzleIterType::Row => self.for_row(index, f),
            PuzzleIterType::Col => self.for_col(index, f),
            PuzzleIterType::Box => self.for_box_index(index, f),
        }
    }

    pub fn for_iter_types() -> [PuzzleIterType; 3] {
        [
            PuzzleIterType::Row,
            PuzzleIterType::Col,
            PuzzleIterType::Box,
        ]
    }

    /// Take one step towards solving the puzzle, if possible. Returns true if it took a step.
    pub fn solve_step(&mut self) -> bool {
        // TODO:(bn) refactor these into distinct rules and outcomes
        // TODO:(bn) also maybe be less lazy and then sort them somehow

        // Find cells that could only be one value
        for r in 0..9 {
            for c in 0..9 {
                let cell = &mut self.cells[r][c];

                if cell.solution.is_some() {
                    continue;
                } // already solved

                if let Some(v) = cell.constraint.solution() {
                    // Cell only has one possible value left, solve it now
                    println!("{v} the only possible value in cell ({r}, {c})");
                    self.solve_cell(r, c, v);
                    return true;
                }
            }
        }

        // find sets where there is a unique value
        for for_iter_type in Puzzle::for_iter_types() {
            for i in 0..9 {
                // For the given set, check if there is only one possible cell for any number.
                let mut uniq = UniqueChecker::new();
                let mut f = |row: usize, col: usize, cell: &mut _| uniq.check(row, col, cell);
                self.for_all(&for_iter_type, i, &mut f);
                //self.for_col(i, &mut f);
                //self.for_box_index(i, &mut f);
                // TODO:(bn) don't call the others if we find a solution early?

                if let Some((r, c, v)) = uniq.found_unique() {
                    println!("found unique {v} in {for_iter_type:?} {i} at ({r}, {c})");
                    self.solve_cell(r, c, v);
                    return true;
                }

                // Check if a box will allow us to exclude values from a row or column
                if for_iter_type == PuzzleIterType::Box {
                    if let Some((exclusion_type, index, v)) = uniq.found_exclusion() {
                        println!("Found that {v} can only be in {exclusion_type:?} {index} based on {for_iter_type:?} {i}");

                        let mut ret = false;
                        self.for_all(
                            &exclusion_type.as_iter_type(),
                            index,
                            &mut |row, col, cell: &mut Cell| {
                                if get_box_index(row, col) != i {
                                    // TODO:(bn) rename these variables!!!
                                    let removed: bool = cell.cant_be(v);
                                    if removed {
                                        ret = true;
                                    }
                                }
                            },
                        );

                        // TODO:(bn) more idiomatic way to do this? Maybe allow for_all to return a type??
                        if ret {
                            return true;
                        }
                    }
                }
            }
        }

        // For hard we need a more complicated version of unique box checking. E.g. box 0 and box 2 both have
        // 3 in rows (0, 1). That means box 1 _must_ have 3 in row 2. So I think the way to do this is pairwise boxes?

        false
    }

    pub fn partial_solution(&self) -> PartialPuzzle {
        let mut ret = PartialPuzzle::new();
        for r in 0..9 {
            for c in 0..9 {
                ret.cells[r][c] = self.cells[r][c].solution;
            }
        }
        ret
    }
}

#[derive(Debug)]
enum UniqueEntry {
    None,
    One(usize, usize),
    SingleRow(usize),
    SingleCol(usize),
    Many,
}

#[derive(Debug)]
struct UniqueChecker {
    map: HashMap<u8, UniqueEntry>,
}

#[derive(Debug)]
enum Exclusion {
    Row,
    Column,
}

impl Exclusion {
    pub fn as_iter_type(&self) -> PuzzleIterType {
        match &self {
            Self::Row => PuzzleIterType::Row,
            Self::Column => PuzzleIterType::Col, // TODO:(bn) consistent naming
        }
    }
}

impl UniqueChecker {
    pub fn new() -> Self {
        let mut ret = Self {
            map: HashMap::new(),
        };

        for i in 1..=9 {
            ret.map.insert(i, UniqueEntry::None);
        }

        ret
    }

    pub fn found_unique(&self) -> Option<(usize, usize, u8)> {
        for (val, uniq) in &self.map {
            if let UniqueEntry::One(r, c) = uniq {
                return Some((*r, *c, *val));
            }
        }
        None
    }

    pub fn found_exclusion(&self) -> Option<(Exclusion, usize, u8)> {
        for (val, uniq) in &self.map {
            match uniq {
                UniqueEntry::SingleRow(i) => return Some((Exclusion::Row, *i, *val)),
                UniqueEntry::SingleCol(i) => return Some((Exclusion::Column, *i, *val)),
                _ => (),
            }
        }
        None
    }

    pub fn check(&mut self, row: usize, col: usize, cell: &mut Cell) {
        if cell.solution.is_some() {
            return;
        }

        for v in cell.constraint().iter() {
            self.map.entry(*v).or_insert(UniqueEntry::None);
            self.map.entry(*v).and_modify(|u| match &*u {
                UniqueEntry::None => *u = UniqueEntry::One(row, col),

                UniqueEntry::One(r, _) if *r == row => *u = UniqueEntry::SingleRow(*r),
                UniqueEntry::One(_, c) if *c == col => *u = UniqueEntry::SingleCol(*c),

                // Check if single row or col with match (and if so no need to update)
                UniqueEntry::SingleRow(r) if *r == row => (),
                UniqueEntry::SingleCol(c) if *c == col => (),

                UniqueEntry::SingleRow(_) => *u = UniqueEntry::Many,
                UniqueEntry::SingleCol(_) => *u = UniqueEntry::Many,
                UniqueEntry::One(..) => *u = UniqueEntry::Many,

                UniqueEntry::Many => (),
            });
        }
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

    /// Removes `val` from the set of values that can be represented, if present and returns true. If not
    /// present, returns false.
    pub fn cant_be(&mut self, val: u8) -> bool {
        self.values.remove(&val)
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

    pub fn iter(&self) -> std::collections::hash_set::Iter<'_, u8> {
        self.values.iter()
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

        assert!(c.cant_be(2));
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
