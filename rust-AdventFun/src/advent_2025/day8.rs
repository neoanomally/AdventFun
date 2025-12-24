use std::collections::{BinaryHeap, HashMap};

use crate::advent_io::advent_file::read2025;


#[derive(Debug, Eq, PartialEq, Hash, Ord, PartialOrd, Clone, Copy)]
struct Circuit {
    x: u32,
    y: u32,
    z: u32
}

#[derive(Debug, Hash, Eq, PartialEq)]
struct CircuitPair {
    left: Circuit,
    right: Circuit
}

impl CircuitPair {
    fn new(left: Circuit, right: Circuit) -> CircuitPair {
        CircuitPair { left, right }
    }

    fn distance(&self) -> f32 {
        let sums = (self.left.x as f32- self.right.x as f32).powi(2) 
            + (self.left.y as f32 - self.right.y as f32 ).powi(2) 
            + (self.left.z as f32 - self.right.z as f32).powi(2);

        sums.sqrt()
    }

    fn wall_distance(&self) -> u32 {
        self.left.x * self.right.x
    }
}

impl Ord for CircuitPair {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.distance().total_cmp(&other.distance())
    }
}

#[allow(clippy::non_canonical_partial_ord_impl)]
impl PartialOrd for CircuitPair {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other).reverse())
    }
}

impl Circuit {
    fn new(x: u32, y: u32, z: u32) -> Circuit {
        Circuit { x, y, z }
    }
}


pub fn run_with_data() {
    let input = read2025("day8_input");
    run_day8(input);
}

#[allow(dead_code)]
pub fn run_with_test_data() {
    let input = read2025("test_day8_input");
    run_day8(input);
}

fn run_day8(input: Vec<String>) {
    let circuits = parse_input(input);

    let heap = create_heap(circuits.clone());
    let (connected_pairs, final_pair) = connect_pairs(circuits, heap);
    let counts = calculate_top_groups(connected_pairs);

    println!("Top Counts: {counts}");
    println!("Final pair: {:?}\nScore: {}", final_pair, final_pair.wall_distance());
}

// Should contain a line of u16
fn parse_input(input: Vec<String>) -> Vec<Circuit> {
    input.iter().map(|line| {
        let split: Vec<u32> = line.split(",")
            .map(|coords| coords.trim().parse::<u32>().expect("Error Parsing input integer"))
            .collect();
        assert_eq!(split.len(), 3);
        Circuit::new(split[0], split[1], split[2])
    }).collect()
}

fn create_heap(circuits: Vec<Circuit>) -> BinaryHeap<CircuitPair> {
    let mut circuit_heap: BinaryHeap<CircuitPair> = BinaryHeap::new();

    for i in 0..circuits.len() {
        for j in i+1..circuits.len() {
            if i == j { continue; }
            circuit_heap.push(CircuitPair::new(circuits[i], circuits[j]));
        }
    }

    circuit_heap
}

fn calculate_top_groups(id_groups: HashMap<u32, Vec<Circuit>>) -> u32 {
    let mut lens: Vec<u32> = id_groups
        .values()
        .map(|value| value.len() as u32 )
        .collect();
        

    lens.sort();
    lens.reverse();
    let mut accum = 1;
    // println!("LENS: {:?}", lens);
    for i in lens.iter().take(3) {
        accum *= i;
    }

    accum
}

fn connect_pairs(circuits: Vec<Circuit>, mut pairs: BinaryHeap<CircuitPair>) -> (HashMap<u32, Vec<Circuit>>, CircuitPair) {
    let mut circuit_ids: HashMap<Circuit, u32> = HashMap::new();
    let mut id_groups: HashMap<u32, Vec<Circuit>> = HashMap::new();
    let mut last_pair = CircuitPair::new(Circuit::new(0, 1, 1), Circuit::new(1, 1, 1));

    for (idx, circuit) in circuits.into_iter().enumerate() {
        circuit_ids.insert(circuit, idx as u32);
        id_groups.insert(idx as u32, vec![circuit]);
    }


    while  !pairs.is_empty() && id_groups.len() > 1 {
        let next = pairs.pop().unwrap();

        let (left_id, right_id) = retrieve_indexes(next.left, next.right, &circuit_ids);
        last_pair = next;
    
        if left_id.is_some() && right_id.is_some() && left_id.unwrap() != right_id.unwrap() {
            let left_id = left_id.unwrap();

            let right_group: Vec<Circuit> = id_groups.remove(&right_id.unwrap()).unwrap();

            id_groups.entry(left_id).and_modify(|entry| entry.extend(&right_group));

            for entry in right_group {
                circuit_ids.entry(entry).insert_entry(left_id);
            }
        }
    }

    (id_groups, last_pair)
}

fn retrieve_indexes(left: Circuit, right: Circuit, lookup: &HashMap<Circuit, u32>) -> (Option<u32>, Option<u32>) {
    (lookup.get(&left).copied(), 
     lookup.get(&right).copied())
}

#[cfg(test)]
pub mod day8_tests {
    use super::*;

    #[test]
    fn test_parse_input() {
        let test_input = read2025("test_day8_input");
        let  circuits = parse_input(test_input);
        let mut ciruit_heap: BinaryHeap<CircuitPair> = create_heap(circuits.clone());


        let expected_left = Circuit::new(162, 817, 812);
        let expected_right = Circuit::new(425, 690, 689);
        let expected_pair = CircuitPair::new(expected_left, expected_right);

        let popped = ciruit_heap.pop().unwrap();
        assert_eq!(expected_pair, popped);
    }

    #[test]
    fn test_connect_pairs() {
        let test_input = read2025("test_day8_input");
        let  circuits = parse_input(test_input);
        let ciruit_heap: BinaryHeap<CircuitPair> = create_heap(circuits.clone());

        let (connected_pairs, final_pair) = connect_pairs(circuits, ciruit_heap);
        let score = calculate_top_groups(connected_pairs);

        assert_eq!(final_pair.wall_distance(), 25272);
        assert_eq!(score, 20);
    }
}
