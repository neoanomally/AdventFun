use std::usize;

use crate::advent_io::advent_file;

struct Grid {
    value: Vec<Vec<char>>
}

impl Grid {
    fn clone(&self) -> Grid {
        Grid { value: self.value.clone() }
    }

    fn max_x(&self) -> i32 {
      self.value.len() as i32
    }

    fn max_y(&self) -> i32 {
        self.value[0].len() as i32
    }

    fn is_roll(&self, x_loc: i32, y_loc: i32) -> bool {
        self.value[x_loc as usize][y_loc as usize] == '@'

    }

    fn remove_roll(&mut self, x_loc: usize, y_loc: usize) {
        self.value[x_loc][y_loc] = 'X'
    }

    fn remove_and_count_valid(&self) -> i64 {
        let mut grid_copy = self.clone();

        let mut sum = 0;

        let mut has_incremented_count = true;
        let mut trip_wire = 0;

        while has_incremented_count && trip_wire < 1000 {
            trip_wire += 1;
            has_incremented_count = false;

            for x in 0..grid_copy.max_x(){
                
                for y in 0..grid_copy.max_y() {
                    if grid_copy.is_roll(x, y) && can_move(&grid_copy, x, y) {
                        sum += 1;
                        grid_copy.remove_roll(x as usize, y as usize);
                        has_incremented_count = true;
                    }
                }
            }
        }

        if trip_wire >= 1000 {
            panic!("ERROR: Trip wire went over 1000 times");
        }
        sum
    }

    fn count_all_valid_rolls(&self) -> i64 {
        println!("Max  X: {}\tMax Y: {}", self.max_x(), self.max_y());
        let mut sum = 0;
        for x in 0..self.max_x() {
            for y in 0..self.max_y() {
                if self.is_roll(x, y) && can_move(self, x, y) {
                    sum += 1;
                }
            }
        }

        sum
    }
}

#[allow(dead_code)]
pub fn run_with_test_data() {
    let input = advent_file::read2025("test_day4_input");
    let parsed_input = parse_input(input);
    let total_can_move = parsed_input.count_all_valid_rolls();

    let total_can_move_part_two = parsed_input.remove_and_count_valid();

    println!("Total tha can move: {}", total_can_move);
    println!("Total numbers that can move part two: {}", total_can_move_part_two);
}


pub fn run_with_data() {
    let input = advent_file::read2025("day4_input");
    let parsed_input = parse_input(input);
    let total_can_move = parsed_input.count_all_valid_rolls();
    let total_can_move_part_two = parsed_input.remove_and_count_valid();
    println!("Total tha can move: {}", total_can_move);
    println!("Total numbers that can move part two: {}", total_can_move_part_two);
}

fn parse_input(input: Vec<String>) -> Grid {
    let grid_value = input.iter()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect();

    Grid { value: grid_value }
}

fn can_move(grid: &Grid, x_loc: i32, y_loc: i32) -> bool {
    let total_neighbors = num_neighbors(grid, x_loc, y_loc);
    total_neighbors < 4
}

fn num_neighbors(grid: &Grid, x_loc: i32, y_loc: i32) -> u8 {
    let mut count = 0;
    for x in (x_loc - 1)..=(x_loc + 1) {
        for y in (y_loc - 1)..=(y_loc + 1) {
            if x == x_loc && y == y_loc {
                continue;
            }

            let is_x_inbound = x >= 0 && x < grid.max_x();
            let is_y_inbound = y >= 0 && y < grid.max_y();
            
            if is_x_inbound && is_y_inbound && grid.value[x as usize][y as usize] == '@' {
                count += 1;
            }
        }
    }

    count
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_num_neighbors() {
        // ..@@
        // @@@.
        // @@@@
        let test_input: Vec<String> = vec!["..@@".to_string(),"@@@.".to_string(),"@@@@".to_string()];
        let grid = parse_input(test_input);

        let should_be_two = num_neighbors(&grid, 0, 0);
        let should_be_none = num_neighbors(&grid, -1 , -1);
        let should_be_five = num_neighbors(&grid, 1, 1);
        let should_be_one_as_well = num_neighbors(&grid, grid.max_x(), grid.max_y());

        assert_eq!(should_be_two, 2);
        assert_eq!(should_be_none, 0);
        assert_eq!(should_be_one_as_well, 1);
        assert_eq!(should_be_five, 6);
    }
}
