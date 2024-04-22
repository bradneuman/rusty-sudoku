use std::error::Error;
use sudoko::PartialPuzzle;
use sudoko::Puzzle;

fn main() -> Result<(), Box<dyn Error>> {
    let input = PartialPuzzle::from_file("medium.csv")?;

    println!("{}", input);

    let mut puzzle = Puzzle::new(&input);
    println!("{}", puzzle);

    if puzzle.solve_step() {
        println!("{}", puzzle);
    }

    Ok(())
}
