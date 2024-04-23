use std::error::Error;
use sudoko::PartialPuzzle;
use sudoko::Puzzle;

fn main() -> Result<(), Box<dyn Error>> {
    let input = PartialPuzzle::from_file("medium.csv")?;

    println!("{}", input);

    let mut puzzle = Puzzle::new(&input);
    println!("{}", puzzle);

    for steps in 1..1000 {
        if !puzzle.solve_step() {
            println!("done after {steps} steps.\n{}", puzzle);
            break;
        }
    }

    println!("Solution:\n{}", puzzle.partial_solution());

    Ok(())
}
