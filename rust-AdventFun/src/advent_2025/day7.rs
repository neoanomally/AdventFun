use std::{collections::HashSet, usize};

use crate::advent_io::advent_file::read2025;

#[derive(Debug, PartialEq, Eq)]
enum Space {
    Empty,
    Splitter,
    Beam
}

#[allow(dead_code)]
struct Teleporter {
    grid: Vec<Vec<Space>>,
    active_splitters: HashSet<Loc>
}

impl Teleporter {
    #[allow(dead_code)]
    fn print_grid(&self) {
        for i in &self.grid {
            for entry in i {
                match entry {
                    Space::Beam => print!("|"),
                    Space::Empty => print!("."),
                    Space::Splitter => print!("^")
                }
            }
            println!("");
        }
    }

    fn coun_num_splits(&self) -> u32 {
        let mut count = 0;

        for y in 0..self.grid.len() {
            for x in 0..self.grid[0].len() {
                match self.grid[y][x] {
                    Space::Splitter => {
                        let up = y - 1;
                        if self.grid[up][x] == Space::Beam {
                            count += 1;
                        }
                    },
                    _ => ()
                }
            }
        }

        count
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
struct Loc {
    x: usize,
    y: usize
}


pub fn run_with_data() {
    let input = read2025("day7_input");
    let teleporter = run_day7(input);
    let num_active_splitters = teleporter.coun_num_splits();
    println!("The number of active Splitters: {}", num_active_splitters);
}


#[allow(dead_code)]
pub fn run_with_test_data() {
    let input = read2025("test_day7_input");
    let teleporter = run_day7(input);
    let num_active_splitters = teleporter.coun_num_splits();
    println!("The number of active Splitters: {}", num_active_splitters);
}

/// There are two ways to do this. The first is by doing a graph traversal. We can start by going
/// left to right and for each line above or S drop it one below. OR just not do a graph traversal.
/// Drop below if it's a . HOWEVER if it's a ^ ignore until I come to the carrot and check if the
/// carrot above has a line. This works beecause there is one empty line each rung of the tree. 
/// add that carrot to the result set
fn run_day7(input: Vec<String> ) -> Teleporter {
    let mut splitters: HashSet<Loc> = HashSet::new();
    let mut grid = parse_input(input);

    for y in 0..grid.len() - 1 {
        for x in 0..grid[0].len() {
            let down = y + 1;
            match grid[y][x] {
                Space::Beam => {
                    if grid[down][x] == Space::Empty {
                        grid[down][x] = Space::Beam; 
                    }
                },
                Space::Splitter => {
                    let up = y - 1;
                    if grid[up][x] == Space::Beam {
                        splitters.insert(Loc{ x: y, y: x});
                    }
                    let left = x - 1;
                    let right = x + 1;

                    grid[down][left] = Space::Beam;
                    if right < grid[0].len() {
                        grid[down][right] = Space::Beam;
                    }
                },
                Space::Empty => { },
            }
        }
    }


    Teleporter {
        grid: grid,
        active_splitters: splitters
    }
}


fn parse_input(input: Vec<String>) -> Vec<Vec<Space>> {
    input.iter().map(|line| line.chars()
        .map(|c| match c {
            '|' | 'S'   => Space::Beam,
            '.'         => Space::Empty,
            '^'         => Space::Splitter,
            value       => panic!("ERROR faound invalid character: {}", value)
        }).collect()).collect()
}



#[cfg(test)]
mod tests_day7 {
    use super::*;


    #[test]
    fn test_parse_input() {
        let test_input = read2025("test_day7_input");
        let grid = parse_input(test_input);

        assert_eq!(grid[0][7], Space::Beam);
    }

    #[test]
    fn test_run_day7() {
        let test_input = read2025("test_day7_input");

        let updated_grid = run_day7(test_input);
        // updated_grid.print_grid();
        let num_active = updated_grid.active_splitters.len();

        assert_eq!(num_active, 21);
    }
}
