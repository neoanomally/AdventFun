use std::{collections::{ HashMap, VecDeque}, vec};

use crate::advent_io::advent_file::read2025;

#[allow(dead_code)]
pub fn run_with_test_data() {
    let input = read2025("test_day10_input");
    run_day10(input);
}

pub fn run_with_data() {
    let input = read2025("day10_input");
    run_day10(input);
}


fn run_day10(input: Vec<String>) {
    let schematics = parse_input(input);

    let sum_schematic_min_pushes = schematics.iter()
        .fold(0, |acc, schematic| acc + schematic.shortest_indicator_presses());

    let sum_schematic_joltage_pushes = schematics.iter()
        .map(|s| {
            let presses = s.shortest_joltage_presses();
            println!("Number of presses for schematic {presses}");
            presses
        })
        .fold(0_u64, |acc, e| acc + e as u64);

    println!("THe sum of the shortest push set is: {sum_schematic_min_pushes}");
    println!("THe sum of the shortest push set for joltage part 2 is: {sum_schematic_joltage_pushes}");
}

// Input will always be [.#..#] (1,2,3) (3, 4, 5) {1, 2, 3} 
// The first segment represents the indicator lights  (always the first one). 
// each round parens is a button
// The Curly bracket is the joltage (always at the end)
fn parse_input(input: Vec<String>) -> Vec<Schematic> {
    input.iter().map(|line| {
        let mut split_iter = line.split(" ").peekable();
        let mut buttons: Vec<String> = Vec::new();
        let indicator: String = split_iter.next().unwrap().to_string();
        
        while let Some(button) = split_iter.next_if(|f| f.starts_with("(")) {
            buttons.push(button.to_string());
        }

        let joltage: String = split_iter.next().unwrap().to_string();

        Schematic::new(indicator, buttons.iter().map(|s| s.to_string()).collect(), joltage)
    }).collect()
}

#[derive(Debug)]
struct Schematic {
    indicator: Vec<bool>,
    buttons: Vec<Vec<usize>>,
    joltage: Vec<u32>
}

struct InProcess { 
    pattern: Vec<bool>,
    seen: Vec<bool>,
    num_clicks: u32
}


#[derive(Eq, PartialEq, Debug, Clone)]
struct Clicks {
    total_clicks: u32,
    joltage_count: Vec<u32>
}

impl Clicks {
    fn new(remaining_joltage: Vec<u32>) -> Clicks {
        Clicks { joltage_count: remaining_joltage , total_clicks: 0 }
    }

    #[allow(dead_code)]
    fn increment_pattern(&self, pattern: &[bool], num_clicks: &u32) -> Option<Clicks> {
        let mut updated_joltage  = self.joltage_count.clone();

        for (idx, value) in pattern.iter().enumerate() {
            if *value && self.joltage_count[idx] == 0 {
                return None;
            } else if *value {
                updated_joltage[idx] -= 1;
            }
        }

        Some(Clicks { joltage_count: updated_joltage, total_clicks: self.total_clicks + num_clicks } )
    }

    fn get_indicator_lights(&self) -> Vec<bool> {
        self.joltage_count.iter().map(|c| c % 2 != 0).collect()
    }
}

impl Ord for Clicks {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let sum_left = self.joltage_count.iter().sum::<u32>();
        let sum_other = other.joltage_count.iter().sum::<u32>();

        u32::cmp(&(sum_other + other.total_clicks), &(sum_left + self.total_clicks))
    }
}

impl PartialOrd for Clicks {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}


impl InProcess { 
    fn new(num_buttons: usize, num_voltages: usize) -> InProcess { 
        InProcess { pattern: vec![false; num_voltages], seen: vec![false; num_buttons], num_clicks: 0 } 
    }

    /// This is an immutable method that does zero changes to the existing data structure.
    /// It clones what has been seen and what has been clicked and then it does a bit wise
    /// or on the pattern 
    fn add_button_click(&self, button_clicked: usize, buttons_added: &Vec<usize>) -> InProcess {
        let mut updated_pattern = self.pattern.clone();
        let mut updated_seen = self.seen.clone();

        for idx in buttons_added {
            updated_pattern[*idx] = true;
        }

        updated_seen[button_clicked] = true;

        InProcess {
            pattern: updated_pattern,
            seen: updated_seen,
            num_clicks: self.num_clicks + 1
        }
    }

    /// This method makes a copy of the existing structure and adds a button as seen but
    /// doesn't increment the click or update the pattern. 
    fn add_button_not_clicked(&self, button_clicked: usize) -> InProcess {
        let mut updated_seen = self.seen.clone();
        updated_seen[button_clicked] = true;

        InProcess {
            pattern: self.pattern.clone(),
            seen: updated_seen,
            num_clicks: self.num_clicks
        }
    }
}

impl Schematic {
    fn new(indicator: String, buttons: Vec<String>, joltage: String) -> Schematic {
        
        let indicator = remove_front_and_back(indicator);
        let schem_indicator: Vec<bool> = indicator.chars()
            .map(|ind| ind == '#')
            .collect();

        let schem_joltage: Vec<u32> = remove_front_and_back(joltage).split(",")
            .map(|j| j.parse::<u32>().unwrap())
            .collect();

        let schem_buttons: Vec<Vec<usize>> = buttons.into_iter().map(|button| {
            remove_front_and_back(button).split(",")
                .map(|b| b.parse::<usize>().unwrap())
                .collect()
        }).collect();

        Schematic {
            indicator: schem_indicator,
            buttons: schem_buttons,
            joltage: schem_joltage
        }
    }

    // should probably
    fn presses_for_indicator(&self, indicator: &Vec<bool>) -> Option<Clicks> {
        println!("SEARCHING FOR {indicator:?}");
        let mut queue: VecDeque<Clicks> = VecDeque::new();
        let mut max_iterations = 0;
        let empty_joltage = vec![0; self.joltage.len()];

        queue.push_back(Clicks::new(empty_joltage));

        while max_iterations < 40000 {
            let mut curr_click = queue.pop_front().unwrap();
            curr_click.total_clicks += 1;

            for button in self.buttons.iter() {
                max_iterations += 1;
                let mut copied_clicks = curr_click.clone();
                for switch_idx in button.iter() {
                    copied_clicks.joltage_count[*switch_idx] += 1;
                }

                if copied_clicks.get_indicator_lights() == *indicator {
                    return Some(copied_clicks);
                } else {
                    queue.push_back(copied_clicks);
                }

            }
        }
        println!("HIT MAXIMUM ITERATIONS");

        None
    }

    /// Not sure I like this method because it could theoretically go on forever and creating this
    /// huge tree. One thing I could think about doing is creating "fused button pushes. This could
    /// be the different combinations of button pushes up to a certain click point. Each of those
    /// could generate a given state from blank. 
    fn shortest_indicator_presses(&self) -> u32 { 
        let mut queue: VecDeque<(Vec<bool>, u32)> = VecDeque::new(); 
        let off_indicator = vec![false; self.indicator.len()];
        queue.push_back((off_indicator, 0));

        let mut max_iterations = 0;

        // The number chosen for max_iterations is arbitrary to prevent from issues. 
        while max_iterations < 400000 {
            let (curr_indicator, depth) = queue.pop_front().unwrap();
            let next_depth = depth + 1;
            for button in self.buttons.iter() {
                let mut copied_indicator = curr_indicator.clone();
                for switch_idx in button.iter() {
                    copied_indicator[*switch_idx] = !copied_indicator[*switch_idx];
                }

                if copied_indicator == self.indicator {
                    // Some people may hate this but there are only two exits; here and panic. 
                    return next_depth;
                } else {
                    queue.push_back((copied_indicator, next_depth));
                }
            }
            max_iterations += 1;
        }

        panic!("ERROR: hit max iterations set as a safeguard on indicator presses.");
    }


   /// Updates the left with the values from the right
    /// The left is completely changed, so caution you stupid mamal. 
    #[allow(dead_code)]
    fn flip_bits(mut left: Vec<bool>, right: Vec<bool>) {
        assert_eq!(left.len(), right.len());
        for i in 0..left.len() {
            left[i] |= right[i];
        }
    }

    // SHould have some cahced value so I'm not recreating these every time but OH Well. 
    #[allow(dead_code)]
    fn get_bits(&self, button_idx: usize) -> Vec<bool> {
        let button = &self.buttons[button_idx];

        let mut bits: Vec<bool> = vec![false; self.joltage.len()];

        for idx in button {
            bits[*idx] = true;
        }

        bits
    }

    fn shortest_clicks_per_pattern(&self) -> HashMap<Vec<bool>, u32> {
        // We may have some pattenrs that can't be completed by clicking each button once
        // BOTH of these would be better by usingn BITs
        let mut queue: VecDeque<InProcess> = VecDeque::new();
        queue.push_back(InProcess::new(self.buttons.len(), self.joltage.len()));

        // For each queue, I need to know what I've seen and I need to know my current pattern.
        // As soon as I find the matching pattern I stop. 
        for i in 0..self.buttons.len() {
            let current_queue_len = queue.len();
            for _ in 0..current_queue_len {
                let current_button = &self.buttons[i];
                let current_process = queue.pop_front().unwrap();

                queue.push_back(current_process.add_button_click(i, current_button));
                queue.push_back(current_process.add_button_not_clicked(i));
            } 
        }

        queue.pop_back(); // Removing the zero clicks value from the back. 

        let pattern_values = queue.into_iter().fold(HashMap::<Vec<bool>, u32>::new(), |mut acc, x| {
            acc.entry(x.pattern)
                .and_modify(|value| *value = std::cmp::min(x.num_clicks, *value) )
                .or_insert(x.num_clicks);
            acc
        });

        pattern_values
    }

    #[allow(dead_code)]
    fn generate_pattern_combinations(&self) -> Vec<Vec<bool>> {
        let num_joltages = self.joltage.len();
        let mut queue: VecDeque<Vec<bool>> = VecDeque::new();

        queue.push_front(Vec::new());

        for _ in 0..num_joltages {
            let queue_len = queue.len();

            for _ in 0..queue_len {
                let front = queue.pop_front().unwrap();

                let mut on = front.clone();
                let mut off = front.clone();
                on.push(true);
                off.push(false);

                queue.push_back(on);
                queue.push_back(off);
            }
        }

        let all_off = queue.pop_back();
        println!("ALL OFF {all_off:?}");
        queue.into()
    }

    #[allow(dead_code)]
    fn get_pattern(input: Vec<u32>) -> Vec<bool> {
        input.iter().map(|v| v % 2 != 0).collect()
    }

    #[allow(dead_code)]
    fn second_order_patterns(patterns: HashMap<Vec<bool>, u32>) -> HashMap<Vec<u32>, u32> {
        let mut updated: HashMap<Vec<u32>, u32> = patterns.iter().fold(HashMap::new(), |mut acc, (pattern, v)| {
            let u_pattern = pattern.iter().map(|b| if *b { 1 } else { 0 } ).collect();
            acc.insert(u_pattern, *v);
            acc
        });

        let current_keys = updated.clone();

        for (left_pattern, left_clicks) in current_keys.iter() {
            for (right_pattern, right_clicks) in current_keys.iter() {
                let mut new_pattern = Vec::<u32>::new();
                for idx in 0..left_pattern.len() {
                    new_pattern.push(left_pattern[idx] + right_pattern[idx]);
                }

                updated.entry(new_pattern)
                    .and_modify(|f| *f = std::cmp::min(*f, left_clicks + right_clicks))
                    .or_insert(left_clicks + right_clicks);
            }
        }

        updated
    }

    // One of the things i can do is identify different patterns 
    // for example if I have the following joltages: {29,29,11,11,40}
    // Look at different patterns [.....], [#....], [#####] with the number
    // of min clicks for each pattern if it's possible. Where the is possible is 
    // < than the max number of clicks per button. We would have to keep track
    // of the number of clicks per button in the search space. 
    // 
    // TODO: We can use a priority queue that takes the min number of clicks 
    fn shortest_joltage_presses(&self, ) -> u32 {
        let patterns: HashMap<Vec<bool>, Clicks> = self.shortest_clicks_per_pattern()
            .iter()
            .fold(HashMap::new(), |mut acc, (key, value)| {
                let updated_vec = key.iter().map(|f| if *f { 1 } else { 0 }).collect();
                acc.insert(key.clone(), Clicks { joltage_count: updated_vec, total_clicks: *value });
                acc
            });

        println!("Buttons: {:?}\nPatterns:", self.buttons);

        for pattern in patterns.iter() { 
            println!("  |-> {pattern:?}");
        }

        self.recurse(patterns, self.joltage.clone())
    }

    fn recurse(&self, mut patterns: HashMap<Vec<bool>, Clicks>, mut joltages: Vec<u32>) -> u32 {
        if joltages.iter().sum::<u32>() == 0 {
            1
        } else {
            let odds: Vec<bool> = joltages.iter().map(|f| f % 2 != 0).collect();

            let pattern = match patterns.get(&odds) {
                Some(v) => v.clone(),
                None => {
                    let new_pattern = self.presses_for_indicator(&odds)
                        .unwrap_or_else(|| panic!("ERROR PATTERN: {odds:?} does not exist"));

                    patterns.insert(new_pattern.get_indicator_lights(), new_pattern.clone());
                    new_pattern
                }
            };

            for (idx, joltage) in joltages.iter_mut().enumerate() {
                *joltage -= pattern.joltage_count[idx];
                *joltage /=2;
            }


            self.recurse(patterns, joltages) + pattern.total_clicks
        }
    }
}

fn remove_front_and_back(input: String) -> String {
    if input.len() < 3 {
        panic!("ERROR the input {input} is not long enough to remove front and back");
    }
    let mut chars = input.chars();
    chars.next();
    chars.next_back();
    chars.collect()
}


#[cfg(test)]
pub mod day10_tests {
    use super::*;


    #[test]
    fn test_parse_input() {
        let test_input = read2025("test_day10_input");
        let schematics = parse_input(test_input);
        let first = schematics.first().unwrap();

        assert_eq!(schematics.len(), 3);
        assert_eq!(first.buttons.len(), 6);
    }


    #[test]
    fn test_remove_front_and_back() {
        let input = "[#..#]";

        let res = remove_front_and_back(input.to_string());

        assert_eq!(res, "#..#");
    }

    #[test]
    fn test_shortest_indicator_button_press() {
        let input = read2025("test_day10_input");
        let schematics = parse_input(input);

        let first_schematic = schematics.first().unwrap();

        let shortest_button_presses = first_schematic.shortest_indicator_presses();

        assert_eq!(shortest_button_presses, 2);
    }

    #[test]
    fn test_shortest_joltage_button_presse() {
        let input = read2025("test_day10_input");
        let schematics = parse_input(input);

        let first_schematic = schematics.first().unwrap();

        let shortest_joltage_presses = first_schematic.shortest_joltage_presses();
        assert_eq!(shortest_joltage_presses, 10);

    }

    // One of the ways is to have methods for setting and checking certain bits.
    // THen toggling a set of bits at the same time. 
    // #[test]
    // fn test_bit_manipulation() {
    //     let number: usize = 6;
    //
    //     println!("BITS: {number:0b}");
    //
    // }
    #[test]
    fn test_shortest_click_per_pattern() {
        let test_input = read2025("test_day10_input");
        let schematics = parse_input(test_input);
        let first = schematics.first().unwrap();
        
        let pattern_clicks = first.shortest_clicks_per_pattern();

        let should_equal_false = pattern_clicks.contains_key(&vec![false, false, false, false]);
        let len = pattern_clicks.len();

        assert_eq!(len, 11);
        assert!(!should_equal_false);
    }

    #[test]
    fn test_all_patterns() {
        let test_input = read2025("test_day10_input");
        let schematics = parse_input(test_input);
        let first = schematics.first().unwrap();
        let patterns = first.generate_pattern_combinations();

        println!("Patterns: {patterns:?}");
    }
}

