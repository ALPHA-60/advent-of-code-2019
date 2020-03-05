
pub fn solve() {
    let c1 = (273025_u32 .. 767253).filter(|&x| is_ok_password(x)).count();
    println!("Day 04, part 1: {} ", c1);
}

fn is_ok_password(number : u32) -> bool {
    let mut has_repeated_digits = false;
    let mut n = number;
    loop {
        let last_digit = n % 10;
        n = n / 10;
        if n == 0 || n % 10 > last_digit {
            break;
        }
        if n % 10 == last_digit {
            has_repeated_digits = true;
        }
    }
    n == 0 && has_repeated_digits
}
