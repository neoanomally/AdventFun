use std::{collections::{HashMap, HashSet, VecDeque}};

use crate::advent_io::advent_file::read2025;

#[derive(Debug, PartialEq, Eq)]
enum Space {
    Empty,
    Splitter,
    Beam
}

#[allow(dead_code)]
struct Teleporter {
    start: Loc,
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
            println!();
        }
    }

    fn coun_num_splits(&self) -> u32 {
        let mut count = 0;

        for y in 0..self.grid.len() {
            for x in 0..self.grid[0].len() {
                if self.grid[y][x] == Space::Splitter {
                    let up = y - 1;
                    if self.grid[up][x] == Space::Beam {
                        count += 1;
                    }
                }
            }
        }

        count
    }

    // TODO: We need to do a depth first search. I want to first go down one level then the next
    // level and so on and so forth. Once I find a leaf node. 
    // Challenge I need to know how much to assign to the splitter above the fcurrent one. 
    // Another thing we can do is start from bottom to top and then just count the number of
    // realities
    #[allow(clippy::unnecessary_unwrap)]
    fn count_num_timelines(&self) -> u64 {
        let mut realities_cache: HashMap<Loc, u64> = HashMap::new();
        let mut deque: VecDeque<Loc> = VecDeque::new();
        let starting_splitter = self.find_next_splitter(&self.start).unwrap();
        deque.push_front(starting_splitter.clone());

        while !deque.is_empty() {
            let current = deque.pop_front().unwrap();

            if realities_cache.contains_key(&current) {
                continue;
            }

            let left = self.find_next_splitter(&Loc::new(current.x - 1, current.y + 1));
            let right = self.find_next_splitter(&Loc::new(current.x + 1, current.y + 1));

            let left_cache_value = left.as_ref().and_then(|loc| realities_cache.get(loc));
            let right_cache_value = right.as_ref().and_then(|loc| realities_cache.get(loc));

            let left_has_been_searched = left.is_none() || left_cache_value.is_some();
            let right_has_been_searched = right.is_none() || right_cache_value.is_some();

            if left_has_been_searched && right_has_been_searched { 
                let total = left_cache_value.unwrap_or(&1) + right_cache_value.unwrap_or(&1); 
                realities_cache.insert(current, total);
            } else {
                deque.push_front(current);
                if left.is_some() && left_cache_value.is_none() {
                    deque.push_front(left.unwrap());
                }
                if right.is_some() && right_cache_value.is_none() {
                    deque.push_front(right.unwrap());
                }
            }
        }

        *realities_cache.get(&starting_splitter).unwrap_or(&0)
    }

    fn find_next_splitter(&self, loc: &Loc) -> Option<Loc> {
        let mut current_loc = Loc { x: loc.x, y: loc.y };
    
        while self.grid[current_loc.y][current_loc.x] != Space::Splitter {
            current_loc.y += 1;

            if current_loc.y >= self.grid.len() {
                return None;
            }
        }
       
        Some(current_loc)
    }


}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
struct Loc {
    x: usize,
    y: usize
}

impl Loc {
    fn new(x: usize, y: usize) -> Loc {
        Loc { x, y }
    }
}

pub fn run_with_data() {
    let input = read2025("day7_input");
    let teleporter = run_day7(input);
    let num_active_splitters = teleporter.coun_num_splits();
    println!("The number of active Splitters: {num_active_splitters}");

    let total_timelines = teleporter.count_num_timelines();
    println!("The total number of timelines: {total_timelines}");
}


#[allow(dead_code)]
pub fn run_with_test_data() {
    let input = read2025("test_day7_input");
    let teleporter = run_day7(input);
    let num_active_splitters = teleporter.coun_num_splits();
    println!("The number of active Splitters: {num_active_splitters}");
    let total_timelines = teleporter.count_num_timelines();
    println!("The total number of timelines: {total_timelines}");
}

/// There are two ways to do this. The first is by doing a graph traversal. We can start by going
/// left to right and for each line above or S drop it one below. OR just not do a graph traversal.
/// Drop below if it's a . HOWEVER if it's a ^ ignore until I come to the carrot and check if the
/// carrot above has a line. This works beecause there is one empty line each rung of the tree. 
/// add that carrot to the result set
fn run_day7(input: Vec<String> ) -> Teleporter {
    let mut splitters: HashSet<Loc> = HashSet::new();
    let start_x = input[0].clone().find('S').expect("Error finding the Start Index");
    let mut grid = parse_input(input);
    let start = Loc {
        y: 0,
        x: start_x
    };

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
        start,
        grid,
        active_splitters: splitters
    }
}


fn parse_input(input: Vec<String>) -> Vec<Vec<Space>> {
    input.iter().map(|line| line.chars()
        .map(|c| match c {
            '|' | 'S'   => Space::Beam,
            '.'         => Space::Empty,
            '^'         => Space::Splitter,
            value       => panic!("ERROR faound invalid character: {value}")
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
        assert_eq!(updated_grid.start, Loc { x: 7,y: 0 });
    }

    #[test]
    fn test_print_timelines() {
        let test_input = read2025("test_day7_input");

        let updated_grid = run_day7(test_input);
        let should_equal_forty = updated_grid.count_num_timelines();

        assert_eq!(should_equal_forty, 40);
    }
}
