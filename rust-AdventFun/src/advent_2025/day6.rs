use crate::advent_io::advent_file::read2025;

#[derive(Debug, PartialEq, Eq)]
enum Operator {
    Add,
    Multiply
}

// The name is from the problem IYKYK
struct Homework {
    numbers: Vec<Vec<u64>>,
    operators: Vec<Operator>  
}

impl Homework {
    fn calculate_all_totals(&self) -> u64 {
        let mut sum: u64 = 0;

        for i in 0..self.numbers.len() {
            sum += self.calculate_total(i);
        }

        sum
    }

    fn calculate_total(&self, idx: usize) -> u64 {
        let current_operator = &self.operators[idx];
        self.numbers[idx].iter().copied().reduce(|accum, next| {
            match current_operator {
                Operator::Multiply => accum * next,
                Operator::Add => accum + next
        }}).unwrap()
    }
}

#[allow(dead_code)]
pub fn run_with_test_data() {
    let input = read2025("test_day6_input");
    run_with_input(input);
}

pub fn run_with_data() {
    let input = read2025("day6_input");
    run_with_input(input);

}

pub fn run_with_input(input: Vec<String>) {
    let homework = parse_input(&input);
    let homework_two = parse_input_two(&input);
    let totals_one = homework.calculate_all_totals();
    let totals_two = homework_two.calculate_all_totals();

    println!("TOTALS for part1: {totals_one}");
    println!("Totals for part2: {totals_two}");
}

// TODO: 
// 1. Convert the split into something else... Probably a string until rotated as the last value in
//    each rotated value is an operator.
// 2. Rotate the inputs so that the vertical entries become the lines. 
// 3. Do some assertion that each of the lines in the input are the same length. This is more of a
//    guard on validating that all the lines are the exact same size.
fn parse_input(input: &[String]) -> Homework {
    let split: Vec<Vec<String>> = input.iter().map(|l| {
        l.split_whitespace().map(|f| f.to_string()).collect()
    }).collect();

    let mut numbers: Vec<Vec<u64>> = Vec::new();
    numbers.resize(split[0].len(), Vec::new());
    let last_idx = split.len() - 1;

    let mut operators: Vec<Operator> = Vec::new();


    for i in 0..split.len() - 1 {
        for (jidx, _) in split.iter().enumerate() {
        // for j in 0..split[0].len() {
            numbers[jidx].push(
                split[i][jidx].parse::<u64>()
                .unwrap_or_else(|_| panic!("Error Parsing: {}", split[i][jidx]))
            );
 
            let operator: Operator = match split[last_idx][jidx].as_str() {
                "*" => Operator::Multiply,
                "+" => Operator::Add,
                _ => panic!("ERROR PARSING: {}", split[last_idx][jidx])
            };

            operators.push(operator);
        }
    }

    // I could just do all the math while parsing it, but doing this in a functional way for
    // testing purposes. 
    Homework {
        numbers,
        operators
    }


}


/// Parse input part two. I need to find the start of the columns, so what I  need to do is
/// first read the bottom of the file. The start of the column will be an operand, the width of
/// the column is always the number of spaces between each operand. So I can reada in the
/// operand then calculate the width of it. THE Exception is the final column it's equal to the
/// following spaces + 1. 
fn parse_input_two(input: &[String]) -> Homework {
    let flattened: Vec<Vec<char>> = input.iter().map(|line| line.chars().collect()).collect();
    let last_idx = flattened.len() - 1;
    let mut columns: Vec<String> = Vec::new();
    let mut numbers: Vec<Vec<u64>> = Vec::new();
    let mut operators: Vec<Operator> = Vec::new();
    
    for i in (0..flattened[0].len()).rev() {
        let mut curr: String = String::new();

        for (jidx, _) in flattened.iter().take(flattened.len() - 1).enumerate() {
        // for j in 0..flattened.len() - 1 { // flattened[0].len() {
            curr.push(flattened[jidx][i]);
        }

        columns.push(curr);

        let maybe_operator: char = flattened[last_idx][i];
        if maybe_operator != ' ' {
   
            let next_numbers: Vec<u64> = columns.iter()
                .map(|line| line.trim())
                .filter(|line| !line.is_empty())
                .map(|line| line.parse::<u64>().unwrap()).collect();
            
            numbers.push(next_numbers);
            columns.clear();

            match maybe_operator {
                '+' => operators.push(Operator::Add),
                '*' => operators.push(Operator::Multiply),
                _   => panic!("ERROR Parsing Operator")
            };
        } 
    }
    
    Homework {
        numbers,
        operators
    }
}



#[cfg(test)]
mod test {
    use super::*;


    #[test]
    fn test_parse_input() {
        let input = read2025("test_day6_input");

        let homework: Homework = parse_input(&input);
        let expected_first_numbers: Vec<u64> = vec![123, 45, 6];
        let expected_first_operator = Operator::Multiply;

        assert_eq!(&expected_first_numbers, homework.numbers.first().unwrap());
        assert_eq!(expected_first_operator, *homework.operators.first().unwrap());
    }

    #[test]
    fn test_first_calculation() {
        let input = read2025("test_day6_input");
        let homework: Homework = parse_input(&input);

        let first_calculation = homework.calculate_total(0);

        assert_eq!(first_calculation, 33210);
    }

    #[test]
    fn test_parse_input_two() {
        let input = read2025("test_day6_input");
        parse_input_two(&input);

    }
}
