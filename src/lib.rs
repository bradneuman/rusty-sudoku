use std::collections::HashMap;
use std::collections::HashSet;
use std::error::Error;
use std::fmt;

// TODO:(bn) consider making a "value" struct to hold only 1-9.

#[derive(Debug)]
pub enum Cell {
    Solved(u8),
    Unsolved(Constraint),
}

impl Cell {
    pub fn new() -> Self {
        Self::Unsolved(Constraint::new())
    }

    pub fn cant_be(&mut self, val: u8) -> bool {
        if let Cell::Unsolved(c) = self {
            c.cant_be(val)
        } else {
            // Nothing removed if the cell is already solved
            false
        }
    }

    pub fn is_solved(&self) -> bool {
        match self {
            Cell::Solved(_) => true,
            Cell::Unsolved(_) => false,
        }
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
                match c {
                    Cell::Solved(v) => {
                        let mut disp = vec!['.'; 9];
                        set_char_to_digit(&mut disp, *v);
                        write!(f, "{} ", disp.into_iter().collect::<String>())?;
                    }
                    Cell::Unsolved(constraint) => {
                        write!(f, "{} ", constraint)?;
                    }
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

fn get_set_index(set: PuzzleIterSet, r: usize, c: usize) -> usize {
    match set {
        PuzzleIterSet::Row => r,
        PuzzleIterSet::Col => c,
        PuzzleIterSet::Square => ((r / 3) * 3 + (c / 3)) as usize,
    }
}

fn get_square_index(r: usize, c: usize) -> usize {
    get_set_index(PuzzleIterSet::Square, r, c)
}

/// Return the top-left index of the square containing (r,c)
fn get_square_start(index: usize) -> (usize, usize) {
    let row = (index / 3) * 3;
    let col = (index % 3) * 3;
    (row, col)
}
// TODO:(bn) test these utils!!

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub enum PuzzleIterSet {
    Row,
    Col,
    Square,
}

impl PuzzleIterSet {
    pub fn all() -> [PuzzleIterSet; 3] {
        [
            PuzzleIterSet::Row,
            PuzzleIterSet::Col,
            PuzzleIterSet::Square,
        ]
    }
}

// TODO:(bn) some magic for this instead?
impl fmt::Display for PuzzleIterSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
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

pub trait SetCallback {
    /// Called to begin iteration over a set type.
    fn begin(&mut self, _set_type: &PuzzleIterSet) -> () {}

    /// Called when the next outer loop is reached (next set of the given set type).
    fn outer_loop(&mut self, _set_index: usize) -> () {}

    /// Called for each element in the current set index
    fn inner_loop(&mut self, _coords: Coords, _cell: &mut Cell) -> () {}
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
        if let Cell::Solved(old_val) = cell {
            if *old_val != val {
                panic!("Solver error: cell ({row}, {col}) already had solution {old_val}, trying to overwrite with {val}");
            }
            return;
        }

        *cell = Cell::Solved(val);

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

    /// Call F for each item in the 3x3 square around (row,col)
    fn for_square_containing<F>(&mut self, row: usize, col: usize, f: &mut F)
    where
        F: FnMut(usize, usize, &mut Cell) -> (),
    {
        self.for_square_index(get_square_index(row, col), f);
    }

    /// Call F for each item in the square indexed 0..9
    fn for_square_index<F>(&mut self, index: usize, f: &mut F)
    where
        F: FnMut(usize, usize, &mut Cell) -> (),
    {
        let (square_r, square_c) = get_square_start(index);
        for r in square_r..(square_r + 3) {
            for c in square_c..(square_c + 3) {
                f(r, c, &mut self.cells[r][c]);
            }
        }
    }

    /// Iterates over the row, column, then square associated with the given cell.
    fn for_all_sets<F>(&mut self, row: usize, col: usize, f: &mut F)
    where
        F: FnMut(usize, usize, &mut Cell) -> (),
    {
        self.for_row(row, f);
        self.for_col(col, f);
        self.for_square_containing(row, col, f);
    }

    // TODO:(bn) remove some of these if they are unused

    fn for_all<F>(&mut self, iter_type: &PuzzleIterSet, index: usize, f: &mut F)
    where
        F: FnMut(usize, usize, &mut Cell) -> (),
    {
        match iter_type {
            PuzzleIterSet::Row => self.for_row(index, f),
            PuzzleIterSet::Col => self.for_col(index, f),
            PuzzleIterSet::Square => self.for_square_index(index, f),
        }
    }

    pub fn iterate_sets<T: SetCallback>(&mut self, set: &PuzzleIterSet, cb: &mut T) {
        cb.begin(set);
        for index in 0..9 {
            cb.outer_loop(index);
            let mut f =
                &mut |row, col, cell: &mut Cell| cb.inner_loop(Coords::from(row, col), cell);
            self.for_all(set, index, &mut f);
        }
    }

    /// Take one step towards solving the puzzle, if possible. Returns true if it took a step.
    pub fn solve_step(&mut self) -> bool {
        // TODO:(bn) refactor these into distinct rules and outcomes
        // TODO:(bn) also maybe be less lazy and then sort them somehow

        // Find cells that could only be one value
        for r in 0..9 {
            for c in 0..9 {
                let cell = &mut self.cells[r][c];

                if let Cell::Unsolved(constraint) = cell {
                    if let Some(v) = constraint.solution() {
                        // Cell only has one possible value left, solve it now
                        println!("{v} the only possible value in cell ({r}, {c})");
                        self.solve_cell(r, c, v);
                        return true;
                    }
                }
            }
        }

        // Check if there is a unique value that can only appear in one place in a given set.
        for iter_set_type in PuzzleIterSet::all() {
            let mut checker = UniqueValueChecker::new();
            self.iterate_sets(&iter_set_type, &mut checker);

            // TODO:(bn) use coords in more places
            if let Some((r, c, v)) = checker.found_unique() {
                println!("found only one place for {v} in {iter_set_type} at ({r}, {c})");
                self.solve_cell(r, c, v);
                return true;
            }

            // // TEMP:
            // if iter_set_type == PuzzleIterSet::Row {
            //     println!(">>> {:#?}", checker);
            // }
        }
        // TEMP: didn't find the 9 in row 6 in step 4 for some reason.... <--------

        // Check if there are exclusions found by limiting a certain value in one set to overlap with another
        // set (e.g. the only possible 7 in square 2 is in row 1, so row 1 in squares 0 and 1 can't be 7).
        for iter_set_type in PuzzleIterSet::all() {
            let mut checker = UniqueIntersectionChecker::new(iter_set_type);
            self.iterate_sets(&iter_set_type, &mut checker);

            if let Some(intersection) = checker.found_unique_intersection() {
                println!(
                    "Found that the {} in {} {} must be somewhere in {} {}",
                    intersection.value,
                    intersection.set_types[0],
                    intersection.set_indexes[0],
                    intersection.set_types[1],
                    intersection.set_indexes[1]
                );

                let mut found = false;

                for r in 0..9 {
                    for c in 0..9 {
                        // TODO:(bn) overlap is a bit inneficient, because we only actually need to clear out
                        // the one we didn't iterate over. Change this once everything else works.
                        match intersection.overlap(r, c) {
                            Overlaps::None => (),
                            Overlaps::One => {
                                let removed = self.cells[r][c].cant_be(intersection.value);
                                if removed {
                                    found = true;
                                }
                            }
                            Overlaps::Both => (),
                        }
                    }
                }

                if found {
                    return true;
                } else {
                    println!("...but we already knew that");
                }
            }
        }

        {
            // TEMP: hacks
            let mut checker = UniqueValueChecker::new();
            checker.begin(&PuzzleIterSet::Row);
            checker.outer_loop(6);
            for i in 0..9 {
                checker.inner_loop(Coords::from(6, i), &mut self.cells[6][i]);
            }

            println!("{checker:#?}");
        }

        false
    }

    pub fn partial_solution(&self) -> PartialPuzzle {
        let mut ret = PartialPuzzle::new();
        for r in 0..9 {
            for c in 0..9 {
                if let Cell::Solved(v) = self.cells[r][c] {
                    ret.cells[r][c] = Some(v)
                }
            }
        }
        ret
    }
}

// TODO:(bn) reorder all of these
#[derive(Debug)]
enum Overlaps {
    None,
    One,
    Both,
}

// TODO:(bn) use ++ operator?
impl Overlaps {
    pub fn increment(&mut self) {
        match &self {
            Self::None => *self = Self::One,
            Self::One => *self = Self::Both,
            Self::Both => (),
        }
    }
}

#[derive(Debug)]
struct Intersection {
    pub value: u8,
    pub set_types: [PuzzleIterSet; 2],
    pub set_indexes: [usize; 2],
}

impl Intersection {
    pub fn overlap(&self, row: usize, col: usize) -> Overlaps {
        let mut ret = Overlaps::None;
        for i in 0..2 {
            if get_set_index(self.set_types[i], row, col) == self.set_indexes[i] {
                ret.increment();
            }
        }
        ret
    }
}

#[derive(Debug)]
struct UniqueValueChecker {
    map: HashMap<u8, UniqueEntry>,
}

impl UniqueValueChecker {
    pub fn new() -> UniqueValueChecker {
        let mut ret = Self {
            map: HashMap::new(),
        };

        for i in 1..=9 {
            ret.map.insert(i, UniqueEntry::None);
        }

        ret
    }

    pub fn found_unique(&self) -> Option<(usize, usize, u8)> {
        for (val, entry) in &self.map {
            if let UniqueEntry::One(r, c) = entry {
                return Some((*r, *c, *val));
            }
        }
        None
    }
}

impl SetCallback for UniqueValueChecker {
    fn outer_loop(&mut self, _set_index: usize) {
        *self = Self::new();
    }

    fn inner_loop(&mut self, coords: Coords, cell: &mut Cell) {
        if let Cell::Unsolved(constraint) = cell {
            for v in constraint.iter() {
                let entry = self
                    .map
                    .get_mut(v)
                    .expect("Internal error: Missing {v} in map");
                match entry {
                    UniqueEntry::None => *entry = UniqueEntry::One(coords.row, coords.col),
                    UniqueEntry::One(..) => *entry = UniqueEntry::Many,
                    UniqueEntry::Many => (),
                }
            }
        }
    }
}

#[derive(Debug)]
struct UniqueIntersectionChecker {
    outer_loop_set: PuzzleIterSet,
    outer_loop_index: usize,

    // For each value, keep a map of each set type and whether it is unique. Note that outer_loop_set should
    // be missing from the sets map, since it must be unique by definition, we only track if either of the
    // other set types are unique.
    sets: HashMap<u8, HashMap<PuzzleIterSet, UniqueEntry>>,
}

impl UniqueIntersectionChecker {
    pub fn new(outer_loop_set: PuzzleIterSet) -> UniqueIntersectionChecker {
        Self {
            outer_loop_set,
            outer_loop_index: 0,
            sets: Self::default_sets(outer_loop_set),
        }
    }

    // Don't track the type of set we are iterating over, since it should always be unqiue by construction (we
    // only iterate over one of it at a time in the inner loop).
    fn default_sets(
        exclude_set: PuzzleIterSet,
    ) -> HashMap<u8, HashMap<PuzzleIterSet, UniqueEntry>> {
        let mut ret = HashMap::new();

        let mut default_map = HashMap::new();
        for set in PuzzleIterSet::all() {
            if set != exclude_set {
                default_map.insert(set, UniqueEntry::None);
            }
        }

        for i in 1..=9 {
            ret.insert(i, default_map.clone());
        }

        ret
    }

    // TODO:(bn) the intersection name is confusing with the constraint .intersect() method, renmae
    pub fn found_unique_intersection(&self) -> Option<Intersection> {
        for (val, map) in &self.sets {
            for (set, uniq) in &*map {
                if let UniqueEntry::One(r, c) = *uniq {
                    return Some(Intersection {
                        value: *val,
                        set_types: [self.outer_loop_set, *set],
                        set_indexes: [self.outer_loop_index, get_set_index(*set, r, c)],
                    });
                }
            }
        }
        None
    }
}

impl SetCallback for UniqueIntersectionChecker {
    fn begin(&mut self, set_type: &PuzzleIterSet) -> () {
        assert_eq!(self.outer_loop_set, *set_type,
                   "Contract error: UniqueIntersectionChecker created with different set type than iterated over");
    }

    fn outer_loop(&mut self, set_index: usize) -> () {
        self.outer_loop_index = set_index;
        self.sets = Self::default_sets(self.outer_loop_set);
    }

    fn inner_loop(&mut self, coords: Coords, cell: &mut Cell) -> () {
        if let Cell::Unsolved(constraint) = cell {
            for v in constraint.iter() {
                let set = self
                    .sets
                    .get_mut(v)
                    .expect("key {v} should be present at construction");
                for (set_type, uniq) in set {
                    let index = get_set_index(*set_type, coords.row, coords.col);
                    match &*uniq {
                        UniqueEntry::None => *uniq = UniqueEntry::One(coords.row, coords.col),
                        UniqueEntry::One(r, c) if get_set_index(*set_type, *r, *c) == index => (),
                        UniqueEntry::One(..) => *uniq = UniqueEntry::Many,
                        UniqueEntry::Many => *uniq = UniqueEntry::Many,
                        // TODO:(bn) these match arms might be cleaner if they returned a new UniqueEntry instead?
                    }
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum UniqueEntry {
    None,
    One(usize, usize), // row, column
    Many,
}

// TODO:(bn) define types for cell coords and set coords?

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
