use sudoko::PartialPuzzle;
use sudoko::Puzzle;

fn main() {
    let mut input = PartialPuzzle::new();

    input.cells[0][0] = Some(3);
    input.cells[4][7] = Some(8);

    println!("{}", input);

    let mut puzzle = Puzzle::new(&input);
    println!("{}", puzzle);
}
