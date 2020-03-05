mod day_01;
mod day_02;
mod day_03;
mod day_04;
mod day_05;
mod day_06;


use std::fs::File;

fn main() {
    let file = File::open("../day_01_input.txt").expect("Cannot open input");
    day_01::solve_part_one(file);

    let file = File::open("../day_01_input.txt").expect("Cannot open input");
    day_01::solve_part_two(file);

    let file = File::open("../day_02_input.txt").expect("Cannot open input");
    day_02::solve_part_one(file);

    let file = File::open("../day_02_input.txt").expect("Cannot open input");
    day_02::solve_part_two(file);

    let file = File::open("../day_03_input.txt").expect("Cannot open input");
    day_03::solve(file);

    day_04::solve();

    //let file = File::open("../day_05_input.txt").expect("Cannot open input");
    //day_05::solve(file);

    let file = File::open("../day_06_input.txt").expect("Cannot open input");
    day_06::solve(file);
}
