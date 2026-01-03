use crate::advent_io;

pub struct Line {
    value:  Vec<u32>
}

impl Line {
    pub fn new(input: &str) -> Self {
        let digits: Vec<u32> = input.chars()
            .map(|c| c.to_digit(10).unwrap() )
            .collect();

        Line { 
            value: digits
        }
    }

    // The score of a given line is based on the highest value. The value of is by taking two
    // digits from within the line and reading them from left to right. So in a line that is
    // 1215538264... the highest number is 86 121753**8**2**6**4
    // one quick way to do this is to build two arrays, one from left to right and one right to
    // left which gives the higest value up to the pivot number.
    pub fn calculate_score_part_one(&self) -> u32 {
        let len = self.value.len();
        let mut right: Vec<u32> = Vec::with_capacity(len);
        let mut max_total: u32 = 0;
        let mut max_left: u32 = 0;
        let mut max_right: u32 = 0;

        right.resize(self.value.len(), 0);
       
        let mut max_seen = 0;
        for i in (0..len).rev() {
            right[i] = max_seen;

            max_seen = std::cmp::max(max_seen, self.value[i]);
        }

        #[allow(clippy::needless_range_loop)]
        for i in 0..(len - 1) {
            if self.value[i] > max_left {
                max_left = self.value[i];
                max_right = right[i];
                max_total = (max_left * 10) + max_right;
            } else if right[i] > max_right {
                max_right = right[i];
                max_total = (max_left * 10) + max_right;
            }
        }

        // let value_as_str: String = self.value.iter().map(|m| m.to_string()).collect();
        // println!("SCORE for {}\t equals: {}", value_as_str, max_total);

        max_total
    }

    // Similar to part one except instead of turning on two switches; we turn on 12. So we'll need
    // to find the highest score to the right and jump to that index. If there are less than N
    // items left. We turn them on one at a time starting with the left highest first if it's
    // possible. Ensure that there is M items available to the right to complete the full 12. 
    // One way to do this is via dynamic programming. We can construct an array so that at any of
    // each point we know the highest value up to point 
    pub fn calculate_score_part_two(&self) -> u64 {
        // could have a more intelligent way to kill the program if the input length isnt as long
        // as the 12 required for part two of the problem. HOwever this is lazier. 
        if self.value.len() < 12 {
            panic!("Error: The input should be at least 12 characters in length");
        }

        // can do this more eficiently by not keeping a vec
        let mut results: Vec<u32> = Vec::new();
        let mut map = self.build_map();
        let mut last_index_removed = -1;

        let mut trip_wire = 0; // this is just for safety
        // This will break if the input is too small. Gonna have a guard condition that just
        // panics above if the self.value is too small. This loop would continue forever.  
        while results.len() < 12 && trip_wire < 20 {
            // println!("RESULTS: {:?}", results);
            // println!("Remaining 9s: {:?}\tRemaining 8s: {:?}", map[9], map[8]);
            // if we have a length of 15 and a results of 0 then we have 
            let max_index: u32 = (self.value.len() - 12 + results.len()) as u32;
            trip_wire += 1;
            // This shoul dbe 0... representing the number. 
            for idx in (0..map.len()).rev() {
                let success: bool = match map[idx].iter().position(|&v| v as i32 > last_index_removed && v <= max_index) {
                    Some(pos) => {
                        results.push(idx as u32); // the IDX is 
                        last_index_removed  = map[idx].remove(pos) as i32;
                        true
                    },
                    _ => false
                };

                if success {
                    break;
                }
            }
        }

        if trip_wire >= 20 {
            panic!("ERROR you have an infinite loop somewhere");
        }
        // println!("REsults: {:?}", results);
        Self::process_result(results)
    }

    // Just take the 10^(12-idx) * value <-- this will move the value to the place so first index
    // should be 0 ... sso (10^12) * 
    fn process_result(input: Vec<u32>) -> u64 {
        const TEN: u64 = 10;
        let num_positions: u32 = input.len() as u32;

        input.iter().enumerate()
            .fold(0, |acc, (idx, &value)| {
                let current = (TEN.pow(num_positions - 1 - idx as u32)) * value as u64;
                acc + current
            })
    }

    // I can build this right to left by finding the higest value to the left by building
    // a map of each number and it's index
    fn build_map(&self) -> Vec<Vec<u32>> {
        let mut index_map: Vec<Vec<u32>> = Vec::new();
        index_map.resize(10, Vec::new());

        for i in 0..self.value.len() {
            let curr_value = self.value[i];
            index_map[curr_value as usize].push(i as u32);
        }

        index_map
    }
}

#[allow(dead_code)]
pub fn run_with_test_data() {
    let contents = advent_io::advent_file::read2025("test_day3_input");

    let lines: Vec<Line> = contents.iter()
        .map(|in_line| Line::new(in_line))
        .collect();

    let total = lines.iter().fold(0_i64, |acc, line| acc + line.calculate_score_part_one() as i64);
    let results_part_two = lines.iter().fold(0, |acc, line| acc + line.calculate_score_part_two());
    println!("Total with test data: {total} ");

    println!("Part Two Results: {results_part_two}");
}

pub fn run_with_data() {
    let contents = advent_io::advent_file::read2025("day3_input");

    let lines: Vec<Line> = contents.iter()
        .map(|in_line| Line::new(in_line)).collect();

    let mut counter = 0;
    let results_part_two = lines.iter().fold(0, |acc, line| {
        // println!("Part two acc: {}", acc);
        counter += 1;
        acc + line.calculate_score_part_two()
    });

    let total = lines.iter().fold(0_i64, |acc, line| acc + line.calculate_score_part_one() as i64);
    println!("Total with full data: {total} ");
    println!("Processed {counter} lines. The results for part two: {results_part_two}");
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_calculate_score() {
        let test_input = "818181911112111".to_string();

        let test_input_2 = "987654321111111".to_string();
        let line = Line::new(&test_input);
        let line_two = Line::new(&test_input_2);
        let score = line.calculate_score_part_one();
        let score_2 = line_two.calculate_score_part_one();
        assert_eq!(score, 92);
        assert_eq!(score_2, 98);
    }

    #[test]
    fn test_calculate_score_two() {
        let test_input = "8879295676472661376616867284483222277533464815636138425232993434286212329432338327371364266252457516".to_string();

        let line = Line::new(&test_input);
        let score_part_two = line.calculate_score_part_two();
        let expected = 999998777516;
        assert_eq!(score_part_two, expected);
    }

}
