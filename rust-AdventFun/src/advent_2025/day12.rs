use std::collections::HashSet;

use crate::advent_io::advent_file::read2025;


pub fn run_with_data() {
    let input = read2025("day12_input");
    run_day12(&input);
}


#[allow(dead_code)]
pub fn run_with_test_data() {
    let input = read2025("test_day12_input");
    run_day12(&input);
}

fn run_day12(input: &[String]) {
    let tree = parse_input(input);

    let total_num_regions_fit_all_presents = tree.count_num_valid_regions();
    println!("The total number of regions that can fit all presents: {total_num_regions_fit_all_presents}");
}


// TODO: Should I have it so I can look if a certain configuration of a present exists within a
// subset? 
// TODO: This might be completely useless :facepalm:
#[allow(dead_code)]
fn generate_present_variations(present: &Present) -> Vec<Present> {
    let mut all_variations: Vec<Vec<Vec<bool>>> = Vec::new();

    let horizontal_flip = y_axis_flip_matrix(&present.shape);
    let vertical_flip = x_axis_flip_matrix(&present.shape);

    all_variations.append(&mut vec![present.shape.clone(), horizontal_flip, vertical_flip]);

    for _ in 0..3 {
        let curr_len = all_variations.len() - 1;
        for idx in 0..3 { 
            let rotataed = rotate_matrix(&all_variations[curr_len - idx]);
            all_variations.push(rotataed);
        }
    }

    let distinct: HashSet<Vec<Vec<bool>>> = HashSet::from_iter(all_variations);
    
    distinct.into_iter()
        .map(|shape| Present { id: present.id, num_spots: present.num_spots, shape })
        .collect()
}

fn parse_input(input: &[String]) -> XTree {
   let mut at_regions = false;  

   let mut input_iter = input.iter();
   let mut regions: Vec<Region> = Vec::new();
   let mut presents: Vec<Present> = Vec::new();

   while let Some(line) = input_iter.next() {
       if line.contains("x") { at_regions = true; } 

       if !at_regions {
           let (id, _) = line.split_once(":").unwrap();
           let grid: Vec<Vec<bool>> = input_iter
               .by_ref()
               .take_while(|next| !next.trim().is_empty())
               .map(|next| next.chars().map(|c| c == '#').collect())
               .collect();

           let num_spots = grid.iter().flatten().filter(|f| **f).count() as u32;
           let id = id.parse::<u32>().unwrap();
           presents.push(Present { id, num_spots, shape: grid});
       } else {
            let (grid_shape, shape_idxes) = line.split_once(": ").unwrap();
            let (width, height) = grid_shape.split_once("x")
                .map(|(l, r)| (l.parse::<u32>().unwrap(), r.parse::<u32>().unwrap()))
                .unwrap();

           let shape_indicies: Vec<u32> = shape_idxes
               .split(" ")
               .map(|f| f.parse::<u32>().unwrap())
               .collect();

           regions.push(Region { width, height, shape_indicies} );
       }
   }

   XTree { regions, presents } 
}


#[allow(clippy::needless_range_loop)]
fn rotate_matrix(shape: &[Vec<bool>]) -> Vec<Vec<bool>> {
    let mut copied_shape: Vec<Vec<bool>> = shape.to_vec();
    let height = shape.len();
    let width = shape.first().map(Vec::len).unwrap();

    for y in 0..height {
        for x in 0..width {
            copied_shape[x][height - y - 1] = shape[y][x];
        }
    }

    copied_shape
}

fn x_axis_flip_matrix(shape: &[Vec<bool>]) -> Vec<Vec<bool>> { 
    let mut copied_shape: Vec<Vec<bool>> = shape.to_vec();
    let height = shape.len();
    let width = shape.first().map(|f| f.len()).unwrap();

    for y in 0..height {
        for x in 0..(width/2) {
            let right_ptr = width - 1 - x;
            copied_shape[y][right_ptr] = shape[y][x];
            copied_shape[y][x] = shape[y][right_ptr];
        }
    }

    copied_shape
}

fn y_axis_flip_matrix(shape: &[Vec<bool>]) -> Vec<Vec<bool>> {
    let mut copied_shape = shape.to_vec();

    let height = shape.first().map(|f| f.len()).unwrap();
    let width = shape.len();

    for x in 0..width {
        for y in 0..height {
            let bottom_ptr = height - 1 - y;
            copied_shape[bottom_ptr][x] = shape[y][x];
            copied_shape[y][x] = shape[bottom_ptr][x];
        }
    }

    copied_shape
}

fn can_all_presents_fit(presents: &[Present], region: &Region) -> bool {
    let area = region.height * region.width;
    let mut total: u32 = 0;
    for (idx, x) in region.shape_indicies.iter().enumerate() {
        total += presents[idx].num_spots * x;
    }


    total <= area
}

struct XTree {
    presents: Vec<Present>,
    regions: Vec<Region>
}

impl XTree { 
    fn count_num_valid_regions(&self) -> u32 {
        self.regions.iter()
            .fold(0, |sum, region| sum + if can_all_presents_fit(&self.presents, region) { 1 } else { 0 })
    }

    /// This method is comparing the max number of presents we can fit under the tree assuming that
    /// each present is a 3x3. So given some area how many 3x3 presents can we fit into that area. 
    #[allow(dead_code)]
    fn count_max_num_presenxts(&self) -> u32 {
        self.regions
            .iter()
            .map(|region| {
                let max_cols = region.height / 3;
                let max_rows = region.width / 3;
                let max_presents = max_cols * max_rows;
                let total_presents = region.shape_indicies.iter().sum();
                max_presents <= total_presents

            }).filter(|f| *f)
                .count() as u32
    }
}

struct Present { 
    id: u32,
    num_spots: u32,
    shape: Vec<Vec<bool>>
}

struct Region { 
    width: u32,
    height: u32,
    shape_indicies: Vec<u32>
}


#[cfg(test)]
mod day12_tests {
    use super::*;

    #[test]
    fn count_all_valid_presesnts() {

    } 
    #[test]
    fn test_parse_input() {
        let input = read2025("test_day12_input");

        let x_tree = parse_input(&input);

        let num_presents = x_tree.presents.len();
        let num_regions = x_tree.regions.len();

        assert_eq!(num_presents, 6);
        assert_eq!(num_regions, 3);
    }

    #[test]
    fn test_num_slots() {
        let input = read2025("test_day12_input");

        let x_tree = parse_input(&input);

        let num_spots_zero = x_tree.presents.first().unwrap().num_spots;

        assert_eq!(num_spots_zero, 7);
    }

    #[test]
    fn test_flip_horizontal_matrix() {

        let original = vec![
            vec![true, false, true, false], vec![false, true, false, true], vec![true, false, true, false], vec![true, true, true, false]
        ];

        let expected = vec![
            vec![true, true, true, false], vec![true, false, true, false], vec![false, true, false, true], vec![true, false, true, false]
        ];

        let flipped = y_axis_flip_matrix(&original);

        assert_eq!(flipped, expected);
    }


    #[test]
    fn test_flip_vertical_matrix() {
        let original = vec![
            vec![true, false, true, false], vec![false, true, false, true], vec![true, false, true, false], vec![true, true, true, false]
        ];

        let expected = vec![
            vec![false, true, false, true], vec![true, false, true, false], vec![false, true, false, true], vec![false, true, true, true]
        ];


        let flipped = x_axis_flip_matrix(&original);

        assert_eq!(flipped, expected);
    }

    #[test] 
    fn test_rotate_matrix() {

        let original = vec![
            vec![true, true, true, true], 
            vec![false, true, false, true], 
            vec![true, false, true, false], 
            vec![true, true, true, false]
        ];

        let expected: Vec<Vec<bool>> = vec![
            vec![true, true, false, true], 
            vec![true, false, true, true],
            vec![true, true, false, true],
            vec![false, false, true, true]
        ];


        let second_original = vec![
            vec![true, true, true],
            vec![true, false, false],
            vec![false, false, false]
        ];

        let second_expected = vec![
            vec![false, true, true],
            vec![false, false, true],
            vec![false, false, true]
        ];

        let rotated = rotate_matrix(&original);
        let second_rotated = rotate_matrix(&second_original);

        assert_eq!(second_rotated, second_expected);
        assert_eq!(rotated, expected);
    }

    #[test]
    fn test_all_rotations() {
        let original = vec![
            vec![true, true, true, true], 
            vec![false, true, false, true], 
            vec![true, false, true, false], 
            vec![true, true, true, false]
        ];

        let present = Present { id: 0, num_spots: 0, shape: original};

        let all_rotations = generate_present_variations(&present);
        assert_eq!(all_rotations.len(), 8);

    }
}
