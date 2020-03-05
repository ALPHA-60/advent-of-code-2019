use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

pub fn solve_part_one(f : File) {
    let reader = BufReader::new(f);

    let total_mass = 
        reader
          .lines()
          .map(|l| l.unwrap().parse::<i32>().unwrap())
          .fold(0, |acc, x| acc + (x / 3) - 2);
    println!("{}", total_mass);
}


pub fn solve_part_two(f : File) {
    let reader = BufReader::new(f);
    let total_mass = 
        reader
          .lines()
          .map(|l| l.unwrap().parse::<i32>().unwrap())
          .fold(0, |acc, x| acc + fuel_required(x));
    println!("{}", total_mass);
}

fn fuel_required(mass : i32) -> i32 {
    let mut total_fuel = 0;
    let mut current_fuel = mass / 3 - 2;
    while current_fuel > 0 {
        total_fuel += current_fuel;
        current_fuel = current_fuel / 3 - 2;
    }
    total_fuel
}

#[test]
fn it_works() {
    assert_eq!(fuel_required(14), 2);
    assert_eq!(fuel_required(1969), 966);
    assert_eq!(fuel_required(100756), 50346);
}
