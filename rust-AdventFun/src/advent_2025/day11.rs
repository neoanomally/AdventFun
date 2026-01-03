use std::collections::{HashMap, VecDeque};

use crate::advent_io::advent_file::read2025;

#[allow(dead_code)]
pub fn run_with_test_data() {
    let input_part2 = read2025("test_day11_part2_input");

    run_day11(&input_part2);
}

pub fn run_with_data() {
    let input = read2025("day11_input");
    run_day11(&input);
}

fn run_day11(input: &[String]) {
    let map = parse_input(input);

    let num_of_paths_out = find_num_paths_points(&map, "you", "out");
    println!("The number of paths out is: {num_of_paths_out}");

    let num_paths_part_2 = find_num_paths_part_2(&map, "svr", "out");
    println!("Number of paths the dumb way: {num_paths_part_2}");

    let really_really_dumb_way = really_really_dumb_way(&map);
    println!("OKAY OKAY OKAY FUCK YOU: {really_really_dumb_way}");

}

#[derive(Clone, Copy, Debug)]
struct PathsOut {
    dac_only_count: u64,
    fft_only_count: u64,
    both_count: u64,
    out_count: u64
}

impl PathsOut {
    fn new(dac_only_count: u64, fft_only_count: u64, both_count: u64, out_count: u64) -> PathsOut {
        PathsOut { dac_only_count, fft_only_count, both_count, out_count }
    }

    fn increment_with(&mut self, dac: u64, fft: u64, both: u64, out: u64) {
        self.out_count += out;
        self.fft_only_count += fft;
        self.dac_only_count += dac;
        self.both_count += both;
    }
}


struct WalkState {
    current_step: String,
    seen: HashMap<String, u32>
}

impl WalkState {
    /// Takes in the previously seen hashset. THis will make a copy of the previously seen HashSet
    /// so no worries about state sharing. j
    fn add_step(step: &str, previously_seen: &HashMap<String, u32>) -> WalkState {
        let mut seen = previously_seen.clone();
        seen.insert(step.to_string(), seen.len() as u32);

        WalkState { current_step: step.to_string(), seen }
    }
}

fn parse_input(input: &[String]) -> HashMap<String, Vec<String>> {
    input.iter()
        .fold(HashMap::new(), |mut acc, line|{
            let (key, values) = line.split_once(": ").expect("ERROR SPLITTING INPUT");
            let values_vec: Vec<String>  = values.split(" ").map(str::to_string).collect();
            acc.insert(key.to_string(), values_vec);

            acc
        })
}


// TODO: Maybe I should have a shortcut that once I find a path every node on that path sets a
// value that it makes it to the end. 
fn find_num_paths_points(map: &HashMap<String, Vec<String>>, start: &str, end: &str) -> u64 {
    let mut queue: VecDeque<WalkState> = VecDeque::new();
    let mut counter = 0;
    let starting_state = WalkState::add_step(start, &HashMap::new());
    queue.push_front(starting_state);

    // Should I include some way to handle loops?
    while !queue.is_empty() {
        let walk_state = queue.pop_front().unwrap();

        if walk_state.current_step == end {
            counter += 1;
            continue;
        }

        let neighbors = match map.get(&walk_state.current_step) {
            Some(n) => n,
            None => continue
        };

        for neighbor in neighbors {
            if !walk_state.seen.contains_key(neighbor) {
                queue.push_front(WalkState::add_step(neighbor, &walk_state.seen));
            } else {
                println!("Neighbor: {neighbor} has already been seen for: {}", walk_state.current_step);
            }
        }
    }

    counter
}


/// Let's have two data structures, 
/// We know we can shortcut a path by saying whether or not a PATH has been seen already, like if
/// we know that a node connects to out AND if that 
/// let's start without htinking about if it goes through two different things. 
fn find_num_paths_part_2(map: &HashMap<String, Vec<String>>, start: &str, end: &str) -> u64 {
    let mut queue: VecDeque<WalkState> = VecDeque::new();
    let starting_state = WalkState::add_step(start, &HashMap::new());
    queue.push_front(starting_state);
    let mut paths_out: HashMap<String, PathsOut> = HashMap::new();

    while !queue.is_empty() {
        let mut walk_state = queue.pop_front().unwrap();
        // WHAT HAPPENS IF WE HIT A NODE WHERE WE HAVNE"T visited OR added the neighbors? 
        let has_no_neighbors = map.get(&walk_state.current_step).is_none();
        if walk_state.current_step == end || paths_out.contains_key(&walk_state.current_step) || has_no_neighbors {
            let end_path = match paths_out.get_mut(&walk_state.current_step) {
                Some(path) => path,
                None => {
                    let dac_count = if walk_state.current_step == "dac" { 1 } else { 0 };
                    let fft_count = if walk_state.current_step == "fft" { 1 } else { 0 };
                    let out_count = if walk_state.current_step == end { 1 } else { 0 };
                    let new_path_out = PathsOut::new(dac_count, fft_count, 0, out_count);
                    paths_out.insert(walk_state.current_step.clone(), new_path_out);
                    paths_out.get_mut(&walk_state.current_step.clone()).unwrap()
                }
            };

            walk_state.seen.remove(&walk_state.current_step);
            let mut seen_as_vec: Vec<(String, u32)> = walk_state.seen.clone().into_iter().collect();
            seen_as_vec.sort_by(|(_, l), (_, r)| r.cmp(l));

            let current_count = end_path.out_count;
            let mut dac_count = end_path.dac_only_count;
            let mut fft_count = end_path.fft_only_count;

            for (node, _) in seen_as_vec {
                if node == "dac" { dac_count = current_count };
                if node == "fft" { fft_count = current_count };
                let both_count = std::cmp::min(dac_count, fft_count) ; 

                paths_out.entry(node)
                    .and_modify(|f| {
                        f.increment_with(dac_count, fft_count, both_count, current_count);
                    })
                    .or_insert(PathsOut::new(dac_count, fft_count, both_count, current_count));
            }

            if walk_state.current_step == end {
                paths_out.remove(end);
            }

            continue;
        }


        let neighbors = match map.get(&walk_state.current_step) {
            Some(n) => n,
            None => {
                &Vec::new()
            }
        };

        for neighbor in neighbors.iter() {
            if !walk_state.seen.contains_key(neighbor) {
                queue.push_front(WalkState::add_step(neighbor, &walk_state.seen));
            } 
        }

    }

    for path in paths_out.iter() {
        println!("PATH: {path:?}");
    }

    let max_key  = paths_out.iter().max_by_key(|f| f.1.both_count);
    println!("MAX BY COUNT: {max_key:?}");

    paths_out.get(start).map(|f| f.out_count).unwrap_or(0)
}

fn really_really_dumb_way(map: &HashMap<String, Vec<String>>) -> u128 {
    // let svr_dac =  find_num_paths_points(map, "svr", "dac");
    // let dac_fft =  find_num_paths_points(map, "dac", "fft");
    // let fft_out =  find_num_paths_points(map, "fft", "out");
// (svr_fft * fft_dac * dac_out) 
    let svr_fft =  find_num_paths_part_2(map, "svr", "fft") as u128;
    let fft_dac =  find_num_paths_part_2(map, "fft", "dac") as u128;
    let dac_out =  find_num_paths_part_2(map, "dac", "out") as u128;

    svr_fft * fft_dac * dac_out
}


#[cfg(test)]
mod day11_tests {
    use super::*;

    #[test]
    fn test_parse_input() {
        let input = vec!["aaa: you hhh".to_string(), "ccc: ddd eee fff".to_string()];

        let map =  parse_input(&input);
        let expected_aaa_values = vec![String::from("you"), String::from("hhh")];
        assert_eq!(map.len(), 2);
        assert_eq!(map.get("aaa"), Some(&expected_aaa_values));
    }

    #[test]
    fn test_find_num_paths_you_out() {
        let input = read2025("test_day11_input");
        let map = parse_input(&input);

        let num_paths_out = find_num_paths_points(&map, "you", "out");
        assert_eq!(num_paths_out, 5);
    }
}
