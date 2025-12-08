use std::i64;

use crate::advent_io;

struct ProductId {
    start:  String,
    end:    String
}

impl ProductId {
    // Tried to do this a weird way where instead of iterating through every possible number; we
    // can know ahead of time that this works only on even lengthed sequences of numbers. Once we
    // know that we can then find the nearest even length number. THe start rounds up and the end
    // portion rounds down to the nearest even lengthed number. Once we have those we can split the
    // left in half and the right in halves. Additional rounding needs to happen; then we just
    // increment once per invalid id. This is very very very efficient and can be done for large
    // ranges so that you don't have to check every number. Once we figure out the splits it should
    // evaluate once per invalid number. So even when checking millions of numbers you might only
    // do 30-40 calculations depending on the ranges and number of ranges.
    fn count_invalid_ids_part_one(&self) -> i64 {
        let mut sum = 0;
        // get the start and end. We need to only take even valuesj
        let updated_start = if self.start.len() % 2 != 0 {
            nearest_len_up(self.start.to_string()).to_string()
        } else { self.start.to_string() };

        let updated_end = if self.end.len() % 2 != 0 {
            &nearest_len_down(&self.end).to_string()
        } else { &self.end };

        // find the midpoint of each the start and the end
        let start_mid = updated_start.len() / 2;
        let end_mid = updated_end.len() / 2;

        // Split the start and ends in half and make 
        // sure the starting positions are valid
        let (start_left, start_right) = updated_start.split_at(start_mid);
        let mut start_left = start_left.parse::<i64>().unwrap();
        let start_right = start_right.parse::<i64>().unwrap();

        if start_left < start_right {
            // print!("Original: {}\tIncrementing the start_left {} -> {}", updated_start, start_left, start_right);
            start_left += 1;
            // println!("\t start_left now: {}", start_left);
        }

        let (end_left, end_right) = updated_end.split_at(end_mid);
        let end_right = end_right.parse::<i64>().unwrap();
        let mut end_left = end_left.parse::<i64>().unwrap();

        if end_left > end_right {
            end_left -= 1;
        }

        while end_left >= start_left {
            sum += extend_half(start_left);
            start_left += 1;
        }

        // Debug purposes only :) 
        // println!("Start: {} - End: {}\t start_left: {} - end_left: {}\tnum Invalid: {}", self.start, self.end, start_left, end_left, count);
        sum
    }

    /// For part one i did a very efficient solution given we always know they are even lengthed
    /// numbers. IN this case where repeating numbers can happen any number of times. For example
    /// 128128128 repeats 128 three different times. We are going to do a similar strategy as part
    /// one but we  will iterate from the start of the range to the end of the range. We do some
    /// math to figure out the length of a number by taking the log of the current number we are
    /// evaluating. Once we have the length we find the mid point which will give us the max number
    /// of times we can split a number. For example 128128128128 is a length of 12, you can have up
    /// to six pairs... We then iterate 1...6 in that example and chexck that number of lengths. We
    /// use modulo and exponentials of 10 to mod off the last n digits. starting with 1... The
    /// right most value is 8, we then repeat it doing some more maths using repeat_n_times. we
    /// know we need to get back to length 12 so we repeat 8 times for the 128 example. Then we do
    /// 28 (repeat it 6 times 282828282828 <- compare it to the original number), then 128 4 times
    ///    and wahla we have a matching number where repeated is equal to the original. Then we add
    ///    that value to the sum and break out of this calculation because we don't want to double
    ///    count in the case of special repeating numbers like 222222. 
    fn count_invalid_ids_part_two(&self) -> i64 {
        let upper_bound = self.end.parse::<i64>().unwrap();
        let start = self.start.parse::<i64>().unwrap();
        let mut sum: i64 = 0;
        
        for current in start..=upper_bound {
            let len = current_len(current);
            let mid = len / 2;

            for x in 1..=mid {
                let divisor = len / x;
                let last_x = last_n_digits(current, x as u32);

                // println!("Current: {}, Divisor: {}\tLast X: {},\tX: {}", current, divisor, last_x, x);

                let repeated = repeat_n_times(last_x, divisor as u32);

                if repeated == current {
                    // println!("INVALID ID: {}\tFrom Range: {} - {}", repeated, self.start, self.end);
                    sum += current;
                    break; // When a pattern repeats at least twice. 
                }

            }
        }

        sum
    }

    fn count_num_values_in_range(&self) -> i64 {
        self.end.parse::<i64>().unwrap() - self.start.parse::<i64>().unwrap()
    }
}

pub fn run_with_data() { 
    let binding = advent_io::advent_file::read2025("day2_input");
    let data = binding.first().unwrap();

    let product_ids = parse_product_ids(data);
    let counts = product_ids.iter().fold(0, |acc, x| acc + x.count_invalid_ids_part_one());
    let total_numbers_checked = product_ids.iter().fold(0, |acc, x| acc + x.count_num_values_in_range());
    let total_numbers_part_two = product_ids.iter().fold(0, |acc, x| acc + x.count_invalid_ids_part_two());

    println!("Sum of invalid ids: {}\t Number of values in Range: {}", counts, total_numbers_checked);
    println!("Number of invalid product ids part 2: {}", total_numbers_part_two);
}

#[allow(dead_code)]
pub fn run_with_test_data() {
    let binding = advent_io::advent_file::read2025("test_day2_input");
    let test_data = binding.first().unwrap();

    let test_product_ids = parse_product_ids(test_data);
    let counts = test_product_ids.iter().fold(0, |acc, x| acc + x.count_invalid_ids_part_one() );

    let total_numbers_checked = test_product_ids.iter().fold(0, |acc, x| acc + x.count_num_values_in_range());
    let total_numbers_part_2 = test_product_ids.iter().fold(0, |acc, x| acc + x.count_invalid_ids_part_two());
    println!("Number of invalid product ids: {}\tNumber of values in Range: {}", counts, total_numbers_checked);
    println!("Number of invalid product ids part 2: {}", total_numbers_part_2);
}

fn parse_product_ids(input: &str) -> Vec<ProductId> { 
    let product_ids: Vec<ProductId> = input.split(",")
        .map(|range| range.split("-").map(|in_str| in_str.parse::<i64>().unwrap()).collect::<Vec<i64>>())
        .map(|split| ProductId { start: split[0].to_string(), end: split[1].to_string()})
        .collect();

    product_ids
}

#[allow(dead_code)]
pub fn find_first_invalid(input: &str) -> i64 {
    // first we need to find if the number is odd. If it's odd then we need to 
    // make it even. THEN we need to check if that number is the upper bound. That can hapen after
    // this return function. 
    let input: String = if input.len() % 2 != 0 {
        // if it's a 1 * 10
        let first_even = (0..input.len()).fold(10, |acc, _| acc * 10);
        first_even.to_string()
    } else { input.to_string() };
    
    let mid = input.len() / 2;
    let (left, right) = input.split_at(mid);

    let left:i64 = left.parse().unwrap();
    let right:i64 = right.parse().unwrap();


    let min_start = std::cmp::max(left, right);

    min_start
}

pub fn current_len(input: i64) -> i64 {
    input.checked_ilog10().unwrap() as i64 + 1
}

pub fn nearest_len_up(input: String) -> i64 {
    const TEN: i64 = 10;
    let len: u32 = input.len().try_into().unwrap();
    TEN.pow(len)
}

pub fn nearest_len_down(input: &String) -> i64 {
    const TEN: i64 = 10;
    let len: u32 = input.len().try_into().unwrap();
    TEN.pow(len - 1) - 1
}

pub fn extend_half(input: i64) -> i64 {
    let len: u32 = input.to_string().len() as u32;
    let multiplier = 10_i64.pow(len) + 1;

    input * multiplier
}

/// This is a fun mehtod. Essentially we want to take any number and repeat it n times. For example
/// if we have 128 and we want to repeat it three times to equal something like 128128128 we can do
/// that using some clever math. We get the length of the current number using ilog10 and add 1 to
/// the value. Then we take 10^(len*repeat_n). Then we get something like 999 or 9,999 or some
/// combinations of 9s then we do the same thing with the denominator but it will give us 9 or 99
/// where it's essentially going to give us some whole number of 1111 when we divide the top by the
/// bottom. Once we have those repeating ones we multiply it by our original number 
/// Example we have 128 which is len 3 and we want to repeat it twice. We'll get 999,999 / 999 = 1001
/// we then multiply 1001 * 128 = 128128. 
pub fn repeat_n_times(x: i64, n: u32) -> i64 {
    const TEN: i64 = 10;
    match x.checked_ilog10() {
        Some(len) => {
            let len = len + 1;
            let numerator = TEN.pow(n * len) - 1;
            let denominator = TEN.pow(len) - 1;
            x * (numerator / denominator)
        },
        None => 0
    }
}

pub fn last_n_digits(x: i64, n: u32) -> i64 {
    const TEN: i64 = 10;

    x % TEN.pow(n)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_up() {
        let should_equal_thousand = nearest_len_up("555".to_string());
        let should_equal_one_hundred_thousand = nearest_len_up("55555".to_string());
        assert_eq!(should_equal_thousand, 1000);
        assert_eq!(should_equal_one_hundred_thousand, 100000);
    }
    
    #[test]
    fn test_dn() {
        let should_equal_ninety_nine = nearest_len_down(&"100".to_string());
        let should_equal_four_nines = nearest_len_down(&"35925".to_string());

        assert_eq!(should_equal_ninety_nine, 99);
        assert_eq!(should_equal_four_nines, 9999);
    }

    #[test]
    fn test_expansion() {
        let should_equal_fifty_five = extend_half(5);
        let should_equal_128128 = extend_half(128);

        assert_eq!(should_equal_fifty_five, 55);
        assert_eq!(should_equal_128128, 128128);
    }

    #[test]
    fn test_repeat_n_times() {
        let should_equal_four_ones = repeat_n_times(1, 4);
        let should_equal_128_three_times = repeat_n_times(128, 3);
        let should_equal_12_three_times = repeat_n_times(12, 3);

        assert_eq!(should_equal_12_three_times, 121212);
        assert_eq!(should_equal_four_ones, 1111);
        assert_eq!(should_equal_128_three_times, 128128128);
    }


    #[test]
    fn test_last_n() {
        let should_equal_212 = last_n_digits(12121212, 3);

        assert_eq!(should_equal_212, 212);
    }
}
