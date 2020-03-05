use std::fs::File;
use std::io::Read;
use std::collections::HashMap;

fn interpret(intcode: &mut Vec<usize>) {
    let mut ip : usize = 0;
    while intcode[ip] != 99 {
        let opcode = intcode[ip];
        let operand1 = intcode[intcode[ip+1]];
        let operand2 = intcode[intcode[ip+2]];
        let dst = intcode[ip+3];
        intcode[dst] = match opcode {
            1 => operand1 + operand2,
            _ => operand1 * operand2,
        };
        ip += 4;
    }
}

pub fn solve_part_one(mut f: File) {
    let mut buffer = String::new();
    f.read_to_string(&mut buffer).expect("error reading file");
    let mut intcode : Vec<usize> =
        buffer.trim_end().split(",").map(|x| {x.parse::<usize>().unwrap()})
        .collect();
    intcode[1] = 12;
    intcode[2] = 2;
    interpret(&mut intcode);
    println!("Day 02, part 1: {}", intcode[0]);
}

pub fn solve_part_two(mut f: File) {
    let mut buffer = String::new();
    f.read_to_string(&mut buffer).expect("error reading file");
    let intcode : Vec<usize> =
        buffer.trim_end().split(",").map(|x| {x.parse().unwrap()})
        .collect();

    let result = |i: usize| {
        let mut intcode = intcode.clone();
        intcode[1] = i / 100;
        intcode[2] = i % 100;
        interpret(&mut intcode);
        intcode[0]
    };

    let result = (0..).find(|&i| result(i) == 19690720);
    println!("Day 02, part 2: {}", result.unwrap());

    

    /*
    x=0;
    y=0;
    l=0;
    let mut hm2 = HashMap::new();
    "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7".split(",").for_each( |s| {
        let dist: i32 = s[1..].parse().unwrap();
        let (dx, dy) = match s.chars().next().unwrap() {
            'U' => (0, 1),
            'R' => (1, 0),
            'D' => (0, -1),
            'L' => (-1, 0),
            _ => panic!()
        };
        for _d in 0..dist {
            x += dx;
            y += dy;
            l += 1;
            hm2.entry((x,y)).or_insert(l);
        }
    });

    for (k,v) in hm2 {
        if (hm.contains_key(&k)) {
            println!("{:?}", (v + hm.get(&k).unwrap()));
        }
    }
    */
       
                                                                

         


}

#[test]
fn it_works() {
    let test_data = vec!(
        (vec!(1,9,10,3,2,3,11,0,99,30,40,50), vec!(3500,9,10,70,2,3,11,0,99,30,40,50)),
        (vec!(1,0,0,0,99),                    vec!(2,0,0,0,99)),
        (vec!(2,4,4,5,99,0),                  vec!(2,4,4,5,99,9801))
    );
    for (mut intcode, output) in test_data {
        interpret(&mut intcode);
        assert_eq!(intcode, output);
    }
}
