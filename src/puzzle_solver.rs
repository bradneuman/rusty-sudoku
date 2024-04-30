use std::collections::HashMap;
use std::fmt;

use crate::util::*;

// TODO:(bn) cleanup
use crate::constraint::*;
use crate::puzzle::*;

// TODO:(bn) split this file up more


// TODO:(bn)  // TODO:(bn)
// TODO:(bn) ok, need a new rule **********************************************************************

// TODO:(bn) in a case where there are only 2 (maybe generalizes?) values in two doubly-linked cells, that
// excludes the other. For example, in box 6 x row 6 there are two cells which could be 3 _or 8. That means
// those two cells "use up" 3 and 8, and other cells in the same box can't be 3 or 8 (e.g. (8,2) can't be 3)

// simpler version of the above might be: the 3 in row 6 must be in column 0 or 1, therefore cells in box 6
// not in those columns can't be 3

// Or, maybe it's "two cells that have identical 2-constraints that share 2 sets exlude others matchin"
// Then, maybe a generalization with 3?

// Maybe I can generalize unique cehcker:
// n=1 -> single value can only exist in one place
// n=2 -> double value can only exist in two places
// n value can only exist in n places (up to 3)

// actually I think it's simpler, (3, 8) by itself, two identical n=2 in a set, means exclude the others from
// that set

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
pub struct PuzzleSolver {
    cells: Vec<Vec<Cell>>,
}

impl fmt::Display for PuzzleSolver {
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

impl PuzzleSolver {
    /// Display each _cell_ as a 3x3 to show constraint values
    pub fn print_large_format(&self) {
        for row in 0..9 {
            for display_row in 0..3 {
                for col in 0..9 {
                    for display_col in 0..3 {
                        let num = (display_row * 3 + display_col + 1) as u8;
                        match &self.cells[row][col] {
                            // Cell::Solved(v) if *v == num => print!("{num} "),
                            // Cell::Solved(_) => print!(". "),
                            Cell::Solved(v) => print!("{} ", *v),
                            Cell::Unsolved(constraint) => {
                                if constraint.contains(num) {
                                    print!("{num} ")
                                } else {
                                    print!("_ ")
                                }
                            }
                        };
                    }
                    print!(" ");
                    if col % 3 == 2 {
                        print!("  ");
                    }
                }
                print!("\n");
            }
            print!("\n");
            if row % 3 == 2 {
                print!("\n\n");
            }
        }
    }
}

// TODO:(bn) move these? Inside impl maybe?
pub fn get_set_index(set: PuzzleIterSet, r: usize, c: usize) -> usize {
    match set {
        PuzzleIterSet::Row => r,
        PuzzleIterSet::Col => c,
        PuzzleIterSet::Square => ((r / 3) * 3 + (c / 3)) as usize,
    }
}

pub fn get_square_index(r: usize, c: usize) -> usize {
    get_set_index(PuzzleIterSet::Square, r, c)
}

/// Return the top-left index of the square containing (r,c)
pub fn get_square_start(index: usize) -> (usize, usize) {
    let row = (index / 3) * 3;
    let col = (index % 3) * 3;
    (row, col)
}

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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ContinueLooping {
    Yes,
    No,
}

pub trait SetCallback {
    /// Called to begin iteration over a set type.
    fn begin(&mut self, _set_type: &PuzzleIterSet) -> () {}

    /// Called when the next outer loop is reached (next set of the given set type). Returns whether or not to
    /// continue looping.
    fn outer_loop(&mut self, _set_index: usize) -> ContinueLooping {
        ContinueLooping::Yes
    }

    /// Called for each element in the current set index
    fn inner_loop(&mut self, _coords: Coords, _cell: &mut Cell) -> () {}
}

impl PuzzleSolver {
    pub fn new(start: &Puzzle) -> PuzzleSolver {
        let mut ret = PuzzleSolver::new_blank();

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

    fn new_blank() -> PuzzleSolver {
        let mut cells: Vec<Vec<Cell>> = Vec::new();

        for r in 0..9 {
            cells.push(Vec::new());
            for _ in 0..9 {
                cells[r].push(Cell::new());
            }
        }

        PuzzleSolver { cells }
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
            if cb.outer_loop(index) == ContinueLooping::No {
                return;
            }
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
        println!("no single cell solution");

        // Check if there is a unique value that can only appear in one place in a given set.
        for iter_set_type in PuzzleIterSet::all() {
            let mut checker = UniqueValueChecker::new();
            self.iterate_sets(&iter_set_type, &mut checker);

            // TEMP:  // TEMP:  // TEMP:  OH OH OH!!! Bug is that I loop the cehcker all the way through so if it finds something in the middle it doesn't early exit.
            // TODO:(bn) need to change to let the checker decide if it should keep going or not!!!
            ///////////////////////////////////////////////////////////////////////////////////

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
        println!("No single set solution");
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
        println!("No overlapping exclusions");

        // TODO:(bn) do I need the other set tyoes?
        // TODO:(bn) I think I can generalize this with UniqueChecker as the n=1 case
        {
            let mut checker = UniqueLinkedCellsChecker::new(PuzzleIterSet::Square);
            self.iterate_sets(&PuzzleIterSet::Square, &mut checker);

            if let Some(exclusion) = checker.found_exclusion() {
                println!("Found that the pair of values ({}, {}) are unique to the locations ({}, {}). Exlcuing others",
                         exclusion.values[0],
                         exclusion.values[1],
                         exclusion.locations[0],
                         exclusion.locations[1]);
                let mut removed = false;
                let mut f = |row, col, cell: &mut Cell| {
                    let coords = Coords::from(row, col);
                    if exclusion.locations.iter().any(|c| *c == coords) {
                        // skip these
                        return;
                    } else {
                        removed = cell.cant_be(exclusion.values[0]) || removed;
                        removed = cell.cant_be(exclusion.values[1]) || removed;
                    }
                };
                
                self.for_square_containing(exclusion.locations[0].row, exclusion.locations[0].col, &mut f);
                if removed {
                    return true;
                } else {
                    println!("...but we already knew that");
                }
            }
            // TEMP:  // TEMP: found next bug!!~
            // TODO:(bn) ********************************************************************************

            // in both this and above where we have a "but we alreayd knew that" case, we need to be able to
            // keep going to the next one. I think maybe the iterate_sets function should be able to continue?
            // Or, it should check this code to determine whether to continue or not once per time the innter
            // loop is done
        }
        println!("No set with 2 cells with the same 2 constraints");
            

        false
    }

    pub fn partial_solution(&self) -> Puzzle {
        let mut ret = Puzzle::new();
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
    fn outer_loop(&mut self, _set_index: usize) -> ContinueLooping {
        // TODO:(bn) maybe make this a trait function instead, might be simpler
        if self.found_unique().is_some() {
            ContinueLooping::No
        } else {
            *self = Self::new();
            ContinueLooping::Yes
        }
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

    fn outer_loop(&mut self, set_index: usize) -> ContinueLooping {
        if self.found_unique_intersection().is_some() {
            return ContinueLooping::No;
        }

        self.outer_loop_index = set_index;
        self.sets = Self::default_sets(self.outer_loop_set);

        ContinueLooping::Yes
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

#[derive(Debug)]
struct UniqueLinkedCellsChecker {
    // Map from the constraint with n=2 to the first coordinate where it was seen
    map : HashMap<Constraint, Coords>,
    result : Option<LinkedExclusion>,
}

#[derive(Debug)]
struct LinkedExclusion {
    values: [u8; 2],
    locations: [Coords; 2],
}

impl<'a> LinkedExclusion {
    pub fn new<I>(iter: &mut I, coords_0: &Coords, coords_1: &Coords) -> Self
        where I: Iterator<Item = &'a u8>,
    {
        Self {
            values: [*iter.next().expect("must have num==2"), *iter.next().expect("must have num==2")],
            locations: [coords_0.clone(), coords_1.clone()],
        }
    }
}

impl UniqueLinkedCellsChecker {
    pub fn new(_iter_type : PuzzleIterSet) -> Self {
        Self{
            map: HashMap::new(),
            result: None
        }
    }

    pub fn found_exclusion(&self) -> Option<&LinkedExclusion> {
        self.result.as_ref()
    }
}

impl SetCallback for UniqueLinkedCellsChecker {
    fn outer_loop(&mut self, _set_index: usize) -> ContinueLooping {
        if self.result.is_some() {
            ContinueLooping::No
        } else {
            self.map = HashMap::new();
            ContinueLooping::Yes
        }
    }

    fn inner_loop(&mut self, coords: Coords, cell: &mut Cell) -> () {
        if self.result.is_some() { return; }
        
        if let Cell::Unsolved(constraint) = cell {
            if constraint.num() == 2 {
                if let Some(first_coords) = self.map.get(constraint) {
                    // Now we have two coordinates with the same n=2 constraint
                    self.result = Some(LinkedExclusion::new(&mut constraint.iter(), first_coords, &coords));
                } else {
                    // TODO:(bn) cleaner with entry.or_insert?
                    self.map.insert(constraint.clone(), coords);
                }
            }
        }
    }
}
    
#[derive(Debug, PartialEq, Clone)]
enum UniqueEntry {
    None,
    One(usize, usize), // row, column
    // TODO:(bn) use coords above?
    Many,
}
