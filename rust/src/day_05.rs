use std::fs::File;
use std::io::{self, Read};

#[derive(PartialEq)]
enum Operation {
    ADD = 1, MULTIPLY,     INPUT,
    OUTPUT,  JUMP_IF_TRUE, JUMP_IF_FALSE,
    LESS,    EQUALS        , HALT = 99
}

use Operation::*;

fn num_to_op(n : usize) -> Operation {
    match n {
        1 => ADD,     2 => MULTIPLY,     3  => INPUT,
        4 => OUTPUT,  5 => JUMP_IF_TRUE, 6  => JUMP_IF_FALSE,
        7 => LESS,    8 => EQUALS,       99 =>  HALT,
        _ => panic!("!")
    }
}

fn get_op(intcode : &Vec<isize>, ip : usize, offset : usize) -> isize {
    let v = intcode[ip + offset];
    if (intcode[ip] / (10isize.pow((offset as u32) +1))) % 10 == 0 {
        intcode[v as usize]
    } else {
        v
    }
}
    
fn interpret(mut intcode: &mut Vec<isize>) {
    let mut ip = 0;

    loop {
        let operation = num_to_op(intcode[ip] as usize % 100);
        match num_to_op(intcode[ip] as usize % 100) {
            ADD | MULTIPLY | LESS | EQUALS  => {
                let dest = intcode[ip+3] as usize;
                let op1 = get_op(&intcode, ip, 1);
                let op2 = get_op(&intcode, ip, 2);
                intcode[dest] = match operation {
                    ADD      => op1 + op2,
                    MULTIPLY => op1 * op2,
                    LESS     => if op1 <  op2 { 1 } else { 0 },
                    EQUALS   => if op1 == op2 { 1 } else { 0 },
                    _ =>     panic!("!")
                };
                ip += 4
            }

            INPUT => {
                
                let mut buffer = String::new();
                io::stdin().read_line(&mut buffer);
                let dest = intcode[ip+1] as usize;
                ip += 2;
                intcode[dest] = buffer.trim_end().parse::<isize>().unwrap()
            },

            OUTPUT  => { println!("{}", get_op(&intcode, ip, 1)); ip += 2; },

            JUMP_IF_TRUE | JUMP_IF_FALSE => {
                let op1 = get_op(&intcode, ip, 1);
                if (operation == JUMP_IF_TRUE && op1 != 0) ||
                    (operation == JUMP_IF_FALSE && op1 == 0) {
                    ip = get_op(&intcode, ip, 2) as usize;
                } else {
                    ip += 3;
                }
            }

            HALT => break
        };
    }
}

pub fn solve(mut f: File) {
    let mut buffer = String::new();
    f.read_to_string(&mut buffer).expect("error reading file");
    let mut intcode : Vec<isize> =
        buffer.trim_end().split(",").map(|x| {x.parse::<isize>().unwrap()})
        .collect();
    interpret(&mut intcode);
    println!("Day 02, part 1: {}", intcode[0]);
}

