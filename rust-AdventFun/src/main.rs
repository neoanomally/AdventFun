use std::env;

mod advent_io;
mod advent_2025;

fn main() { 
    let args: Vec<String> = env::args().collect();
    let mut args_iter = args.iter();
    let mut day = "0";

    // TODO Implement test 
    while let Some(ref mut arg) = args_iter.next() {
        match arg.as_str() { 
            "--day" => {
                day = args_iter.next().expect("Expect a parameter for day");
            },
            value => println!("Arg: {} unexpected", value)
        }
    }

    match day {
        "1" => advent_2025::day1::run_day_one_part_one(),
        "2" => advent_2025::day2::run_with_data(),
        rest => {
            advent_2025::day2::run_with_data();
            println!("Invalid or unimplemented day: {}", rest)
        }
    }
    
}
