use std::fs::File;
use std::collections::HashMap;
use std::io::BufReader;
use std::io::BufRead;
use std::iter::repeat;

fn unit_vector(segment: &str) -> (i32, i32) {
    match segment.chars().nth(0).unwrap() {
        'U' => (0, 1),
        'R' => (1, 0),
        'D' => (0, -1),
        'L' => (-1, 0),
        _ => panic!("crap")
    }
}

// maps coordinate to wire length at that point
fn make_wire(segments: String) -> HashMap<(i32, i32), usize> {
    let mut wire = HashMap::new();
    segments
        .split(",")
        .flat_map(|seg| {
            let distance = seg[1..].parse().unwrap();
            repeat(unit_vector(seg)).take(distance)
        })
        .scan((0,0), |(x,y), (dx, dy)| { *x+=dx; *y+=dy ; Some((*x,*y))}) // coords
        .zip(1..) // lengths
        .for_each(|(coords, len)| { wire.entry(coords).or_insert(len); });
    wire
}

pub fn solve(f: File) {
    let mut lines = BufReader::new(f).lines();
    let wire1 = make_wire(lines.next().unwrap().unwrap());
    let wire2 = make_wire(lines.next().unwrap().unwrap());


    let closest =
        wire1
        .iter()
        .flat_map(|(coords, _)| wire2.get(&coords).map(|_|  coords.0.abs() + coords.1.abs()))
        .min();


    let shortest =
        wire1
        .iter()
        .flat_map(|(coords, len1)| wire2.get(&coords).map(|len2| len1 + len2))
        .min();

    println!("Day 03: part 1: {}", closest.unwrap());
    println!("Day 03: part 2: {}", shortest.unwrap());
}
