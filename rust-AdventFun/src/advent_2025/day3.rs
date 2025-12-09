use crate::advent_io;

pub struct Line {
    value:  Vec<u32>
}

impl Line {
    pub fn new(input: &String) -> Self {
        let digits: Vec<u32> = input.chars()
            .map(|c| c.to_digit(10).unwrap() as u32)
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
    pub fn calculate_score(&self) -> u32 {
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
}

pub fn run_with_test_data() {
    let contents = advent_io::advent_file::read2025("test_day3_input");

    let lines: Vec<Line> = contents.iter()
        .map(|in_line| Line::new(in_line))
        .collect();

    let total = lines.iter().fold(0_i64, |acc, line| acc + line.calculate_score() as i64);
    println!("Total with test data: {} ", total);
}

pub fn run_with_data() {
    let contents = advent_io::advent_file::read2025("day3_input");

    let lines: Vec<Line> = contents.iter()
        .map(|in_line| Line::new(in_line)).collect();

    let total = lines.iter().fold(0_i64, |acc, line| acc + line.calculate_score() as i64);
    println!("Total with full data: {} ", total);
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
        let score = line.calculate_score();
        let score_2 = line_two.calculate_score();
        assert_eq!(score, 92);
        assert_eq!(score_2, 98);
    }

}
