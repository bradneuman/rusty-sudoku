use std::error::Error;
use sudoko::Puzzle;
use sudoko::PuzzleSolver;


fn main() -> Result<(), Box<dyn Error>> {
    let input = Puzzle::from_file("medium.csv")?;

    println!("{}", input);

    let mut puzzle = PuzzleSolver::new(&input);
    println!("{}", puzzle);

    for steps in 1..1000 {
        // TEMP:
        println!("Step {steps}:\n{puzzle}");
        if !puzzle.solve_step() {
            println!("done after {steps} steps.\n{}", puzzle);
            break;
        }
    }

    println!("Solution:\n{}", puzzle.partial_solution());

    Ok(())
}
