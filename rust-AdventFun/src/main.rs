use std::env;

mod advent_io;
mod advent_2025;

fn main() { 
    let args: Vec<String> = env::args().collect();
    let mut args_iter = args.iter();
    args_iter.next();
    let mut day = "0";

    // TODO Implement test 
    println!("Args: {:?}", args);
    while let Some(ref mut arg) = args_iter.next() {
        match arg.as_str() { 
            "day" => {
                day = args_iter.next().expect("Expect a parameter for day");
            },
            value => println!("Arg: {} unexpected. Usage for specific day include --day #", value)
        }
    }

    match day {
        "1" => advent_2025::day1::run_day_one_all(),
        "2" => advent_2025::day2::run_with_data(),
        "3" => advent_2025::day3::run_with_data(),
        "4" => advent_2025::day4::run_with_data(),
        "5" => advent_2025::day5::run_with_data(),
        "6" => advent_2025::day6::run_with_data(),
        "7" => advent_2025::day7::run_with_data(),
        "8" => advent_2025::day8::run_with_data(),
        rest => {
            println!("Invalid or unimplemented --day: {}", rest);
            advent_2025::day8::run_with_data();
        }
    }
    
}
