use crate::advent_io;


pub fn run_day_one_part_one() {
    println!("Running day one part one test data");
    rotate_dial_test_data();
    println!("Running day one part one full data");
    rotate_dial_data();
}

pub fn rotate_dial_data() -> i32 {
    let data = advent_io::advent_file::read2025("day1_input");
    rotate_dial(data)
}


pub fn rotate_dial_test_data() -> i32 { 
    let data = advent_io::advent_file::read2025("test_day1_input");
    rotate_dial(data)
}

pub fn rotate_dial(data: Vec<String>) -> i32 {

    let mut num_exact_zeroes = 0;
    let mut num_all_zeroes = 0;
    let mut current_position: i32 = 50; 

    for line in data { 
        let mut chars: std::str::Chars = line.trim().chars();
        let starting_position = current_position;
        let mut rotated_past = false;
        let direction = chars.next();
        let value: i32= chars
            .as_str()
            .parse()
            .expect("Error parsing the dial values into i32");
        
        let rotations = value / 100;
        num_all_zeroes += rotations;
        // println!("Adding {} zeroes:      from {}", rotations, value);
        let remainder = value % 100;
        if remainder == 0 { println!("REMAINDER == 0"); } 

        // TODO the data has greater than hundreds
        // TODO Check if we rotated past zero. 
        match direction {
            Some('L') => {
                current_position = current_position - remainder;
                if current_position < 0 {
                    current_position = 100 + current_position;
                    if starting_position != 0  { rotated_past = true; }
                } 
            },
            Some('R') => {
                current_position = current_position + remainder;
                if current_position >= 100 {
                    current_position = current_position - 100;
                    rotated_past = true;
                }
                // if starting_position == 0 && current_position < 100 { num_all_zeroes += 1; }
            }, Some(_) | None => ()
        }


        if current_position == 0 {
            num_exact_zeroes += 1;
        }

        if rotated_past || current_position == 0 { num_all_zeroes += 1; } 
    }

    println!("The number of zeroes: {}", num_exact_zeroes);
    println!("The total number of zeroes: {}", num_all_zeroes);
    num_exact_zeroes
}
