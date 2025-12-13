use crate::advent_io::advent_file;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Range {
    start: u64,
    end: u64
}

type ProductId = u64;

struct Ingredients {
    ranges: Vec<Range>,
    products: Vec<ProductId>
}

impl Ingredients{ 
    #[allow(dead_code)]
    fn ensure_sort(&mut self) {
        self.ranges.sort_by_key(|f| f.start);
    }

    // This only works if the ranges are already merged. This should happen on creation. 
    fn contains(&self, value: u64) -> bool {
        let mut trip_wire = 0;
        let mut left = 0;
        let mut right = self.ranges.len(); 
        let mut has_item = false;
        
        while left < right && !has_item {
            trip_wire += 1;
            if trip_wire > 100 {
                panic!("ERROR trip wire for binary search");
            }
            let mid = left + ((right - left) / 2);
            let range = &self.ranges[mid]; 
            // println!("Checking if {} is in between {}     {}", value, range.start, range.end);

            if value >= range.start && value <= range.end {
                has_item = true;
            } else if value < range.start {
                right = mid
            } else {
                left = mid + 1 ; 
            }
        }

        has_item
    }

    // The answer is 638 for part 1... Why didn't the binary search work??? // Answered due to the
    // merge. You need to merge the ranges first; then you'll be able to get the right answer with
    // binary search and that's because there are overlapping ranges and we might pick the wrong
    // one. THat's the way I have the binary search set up
    #[allow(dead_code)]
    fn contains_brute(&self, value: u64) -> bool {
        let mut has_found = false;

        for range in &self.ranges {
            if value >= range.start && value <= range.end {
                has_found = true;
                break;
            }
        }

        has_found
    }

    fn count_num_fresh(&self) -> u32 {
        self.products.iter().fold(0, |acc, value| {
            // println!("Searching for: {} -- found: {}", value, self.contains(*value));
            acc + if self.contains(*value) { 1 } else { 0 } 
        })
    }

    fn distinct_fresh_ingredient_ranges_available(&self) -> u64 {
        self.ranges.iter().fold(0, |accum, range| {
            println!("{:?}\t = {}", range, (range.end - range.start + 1));
            accum + (range.end - range.start) + 1
        }) 
    }
}

pub fn run_with_data() {
    let data = advent_file::read2025("day5_input");
    run_with_input(data);

    println!("Running With Data Day 5");
}


#[allow(dead_code)]
pub fn run_with_test_data() {
    let data = advent_file::read2025("test_day5_input");
    println!("Running With Test Data Day 5");
    run_with_input(data);
}


pub fn run_with_input(input: Vec<String>) {
    let ingredients = parsed_input(input);

    // println!("Parsed Inputs: {:?}", ingredients.ranges.iter().map(|f| format!("{}-{}  ", f.start, f.end)).collect::<String>());
    // println!("Parsed Inputs: {:?}", ingredients.products)
    let num_fresh_ingredients = ingredients.count_num_fresh();
    let num_total_fresh_ids = ingredients.distinct_fresh_ingredient_ranges_available();
    println!("The number of fresh ingredients part 1: {}", num_fresh_ingredients);
    println!("Part two: Total number of fresh ids: {}", num_total_fresh_ids);
}

fn merge_ranges(ranges: Vec<Range>) -> Vec<Range> {
    if ranges.len() < 1 {
        return ranges
    }

    let (first, rest) = ranges.split_first().unwrap();

    let results: Vec<Range> = rest.iter()
        .fold(vec![*first], |mut accum, next| {
            let len = accum.len(); 
            let prev = accum.last().unwrap();
            if (prev.end + 1) >= next.start && prev.start <= next.start {
                // I don't like updating the final value of the accumulator directly but due to the
                // borrow rules it's easier to do it this way than to handle it in a different way. 
                accum[len - 1] = Range {
                   start: std::cmp::min(prev.start, next.start),
                   end: std::cmp::max(prev.end, next.end)
                };
            } else {
                accum.push(*next);
            }

            accum
    });

    results
}

fn parsed_input(input: Vec<String>) -> Ingredients {
    let mut space_detect = false;
    let mut ranges: Vec<Range> = Vec::new();
    let mut products: Vec<ProductId> = Vec::new();

    for line in input {
        let line = line.trim();
        if !space_detect && line.is_empty() {
            space_detect = true;
        } else if !space_detect {
            let (left, right) = line.split_once("-")
                .expect("Error splitting the line: ");

            ranges.push(Range { start: left.parse().unwrap(), end: right.parse().unwrap() });
        } else {
            products.push(line.parse().expect("Error unwrapping product id"));
        }
    }

    ranges.sort_by_key(|f| (f.start, f.end)); 
    let ranges = merge_ranges(ranges);

    Ingredients {
        ranges: ranges,
        products: products
    }
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn check_if_ranges_contains() {
        let test_input: Vec<String> = vec!["10-12", "15-21", "3-5", "", "11", "81"].iter().map(|f| f.to_string()).collect();
        let ingredients = parsed_input(test_input);

        let should_contain = ingredients.contains(11);
        let should_not_contains = ingredients.contains(0);
        let should_also_not_contains = ingredients.contains(14);
        let should_aslo_contain = ingredients.contains(5);
        let should_very_much_contain = ingredients.contains(10);

        assert_eq!(should_contain, true);
        assert_eq!(should_not_contains, false);
        assert_eq!(should_also_not_contains, false);
        assert_eq!(should_aslo_contain, true);
        assert_eq!(should_very_much_contain, true);
    }

    #[test]
    fn test_merge_ranges() {
        let ranges: Vec<Range> =  vec![ Range { start: 1, end: 5 },
            Range { start: 8, end: 12 }, Range { start: 10, end: 15 },
            Range { start: 15, end: 16 } ];

        let merged = merge_ranges(ranges);

        assert_eq!(merged.len(), 2);
        assert_eq!(merged.last().unwrap(), &Range { start: 8, end: 16 });
    }
}

