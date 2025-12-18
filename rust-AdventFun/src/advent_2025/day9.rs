use std::collections::VecDeque;

use crate::advent_io::advent_file::read2025;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct Coord {
    x: u64,
    y: u64
}

impl Coord {
    fn new(x: u64, y: u64) -> Coord {
        Coord { x: x, y: y }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct TileLine {
    start: Coord, 
    end: Coord
}


// let's say I have a 0, 3 to 5,3 AND 2,3 TO 8, 3. <-- we can merge
// 0, 5, 2, 8      5    && 6 
// If I can merge this with that; merge them and return Some(new) else return None
impl TileLine {
    fn new(left: Coord, right: Coord) -> TileLine {
        // NEED TO ACTUALLY check which one is smaller than the other
        let (start, end) = if left < right {
            (left, right)
        } else {
            (right, left)
        };
        
        TileLine {
            start, 
            end
        }
    }

    /// When merging it's better to just do this method; however when you want to check if lines
    /// overlap use the has_overlapping
    fn merge_tile_line(&self, other: TileLine) -> Option<TileLine> {
        if self.has_overlapping_horizontal(&other) {
            let updated_start = Coord { x: self.start.x, y: std::cmp::min(self.start.y, other.start.y) };
            let updated_end = Coord { x: self.start.x, y: std::cmp::max(self.end.y, other.end.y) };

            Some(TileLine{ start: updated_start, end: updated_end })
        } else if self.has_overlapping_vertical(&other) {
            let updated_start = Coord { x: std::cmp::min(self.start.x, other.start.x), y: self.start.y };
            let updated_end = Coord { x: std::cmp::max(self.end.x, other.end.x), y : self.start.y };

            Some(TileLine { start: updated_start, end: updated_end })
        } else {
            None
        }
    }

    fn has_overlapping_horizontal(&self, other: &TileLine) -> bool {
        let all_same_x = self.start.x == other.start.x && self.start.x == other.end.x && self.start.x == self.end.x;
        all_same_x && ((self.start.y >= other.start.y &&  self.end.y >= other.end.y) 
            || (self.end.y >= other.start.y && self.end.y <= other.end.y))
    }

    fn has_overlapping_vertical(&self, other: &TileLine) -> bool {
        let all_same_y = self.start.y == other.start.y && self.start.y == self.end.y && self.start.y == other.end.y;
        all_same_y && ((self.start.x >= other.start.x && self.end.x <= other.end.x)
            || (self.end.x >= other.start.x && self.end.x <= other.end.x))
    }

    
    // TODO: Write tests for this
    fn is_fully_within(&self, other: &TileLine) -> bool {
        let all_same_x = self.start.x == other.start.x && self.start.x == other.end.x && self.start.x == self.end.x;
        let all_same_y = self.start.y == other.start.y && self.start.y == self.end.y && self.start.y == other.end.y;

        if all_same_x {
            self.start.y >= other.start.y && self.end.y <= other.end.y
        } else if all_same_y {
            self.start.x >= other.start.x && self.end.x <= other.end.x
        } else {
            false
        }
    }

    /// The best way to do this is sor them by start and end, then use a queue where the first
    /// element in the queue is the first element in the vector. Then loop through the vector and
    /// see if it can be merged in with the first element within the stack. If so pop the stack and
    /// put the merged on eon and keep doing that over and over and over until the end of the
    /// vector. This will give us O(NlogN) overall as it's a sort then a linear loop. 
    fn merge_all_tiles(mut tiles: Vec<TileLine>) -> Vec<TileLine> {
        if tiles.len() <= 1 {
            return tiles
        }

        tiles.sort();
        let mut tiles_iter = tiles.into_iter();

        let mut deque: VecDeque<TileLine> = VecDeque::new();
        if let Some(first) = tiles_iter.next() {
            deque.push_front(first);
        }

        for tile in tiles_iter {
            let front = deque.pop_front().expect("The Deque should never be empty");

            match front.merge_tile_line(tile.clone()) {
                Some(merged) => deque.push_front(merged),
                None => {
                    deque.push_front(front);
                    deque.push_front(tile);
                }
            }

        }

        deque.into()
    }

}


#[allow(dead_code)]
pub fn run_with_test_data() {
    let input = read2025("test_day9_input");
    run_day9(input);
}

pub fn run_with_data() {
    let input = read2025("day9_input");
    run_day9(input);

}


fn run_day9(input: Vec<String>) {
    let coords = parse_input(input);
    let max_area = find_max_rectangle_area(coords);
    println!("The max area for Part 1: {}", max_area);
}


/// TODO: What I need to do is be able to identify all the coordinates with respect to each other
/// and calculate the total length and height with repsect to each one. This is actually pretty
/// easy
fn find_max_rectangle_area(coords: Vec<Coord>) -> u64 {
    let mut largest_rectangle = 0;

    for i in 0..coords.len() {
        for j in i+1..coords.len() {
            let first_coord = &coords[i];
            let second_coord = &coords[j];

            // if we have 0, 0 and 3, 3 (width = x1 - x2) and height = (
            let height = first_coord.y.abs_diff(second_coord.y) + 1;
            let width = first_coord.x.abs_diff(second_coord.x) + 1;

            let area = height * width;
            // println!("Area: {}\tFirst: {:?}\tSecond: {:?}", area, first_coord, second_coord);
            largest_rectangle = largest_rectangle.max(area);
        }
    }

    largest_rectangle
}

// Really these are creating lines to make sure any rectangle that we create are within existing
// lines. So once we have the set of lines, we can see if any given rectangle lines are within
// green lines. 
// 1. Find all lines
// 2. Merge overlapping lines
// 3. index each line into two hashmaps: Vertical Lookups and Horizontal lookupsjjj
fn create_green_tiles(input: Vec<Coord>) -> Vec<Coord> {
    let mut green_tiles: Vec<Coord> = Vec::new();


    green_tiles
}

fn parse_input(input: Vec<String>) -> Vec<Coord> {
    input.iter().map(|line| {
        let data: Vec<u64> = line.split(",")
            .map(|split| split.parse::<u64>().unwrap())
            .collect();

        Coord { x: data[0], y: data[1] }
    }).collect()
}



#[cfg(test)]
mod day9_tests {
    use super::*;

    #[test]
    fn test_parse_input() {
        let input: Vec<String> = vec!["7,1", "11,1", "9,7"].iter().map(|f| f.to_string()).collect();
        let coords = parse_input(input);


        assert_eq!(coords.first(), Some(Coord { x: 7, y: 1 }).as_ref());
    }


    #[test]
    fn test_find_max_rectangle() {
        let test_inptu = read2025("test_day9_input");
        let coords = parse_input(test_inptu);

        let max_opposing = find_max_rectangle_area(coords);
        assert_eq!(max_opposing, 50);
    }

    fn test_merge_line() { 
        let line_one = TileLine { start: Coord::new(0, 3), end: Coord::new(0, 5) };
        let line_two = TileLine { start: Coord::new(0, 4), end: Coord::new(0, 8) };
 
        let line_three = TileLine { start: Coord::new(3, 0), end: Coord::new(5, 0) };
        let line_four = TileLine { start: Coord::new(4, 0), end: Coord::new(8, 0) };

        let line_five = TileLine { start: Coord::new(0, 5), end: Coord::new(0, 8) };

        let merged_one = line_one.merge_tile_line(line_two);
        let expected_one = TileLine { start: Coord::new(0, 3), end: Coord::new(0, 8) };
        let expected_one_cp = TileLine { start: Coord::new(0, 3), end: Coord::new(0, 8) };

        let merged_two = line_three.merge_tile_line(line_four);
        let expected_two = TileLine { start: Coord::new(3, 0), end: Coord::new(8, 0) };

        let merged_three = line_one.merge_tile_line(line_three);
        let merged_four = line_one.merge_tile_line(line_five);

        assert_eq!(merged_one, Some(expected_one));
        assert_eq!(merged_two, Some(expected_two));
        assert_eq!(merged_three, None);
        assert_eq!(merged_four, Some(expected_one_cp));
    }

    #[test]
    fn test_merge_all_lines() {
        let line_zero = TileLine { start: Coord::new(0, 16), end: Coord::new(0, 21)};
        let line_one = TileLine { start: Coord::new(0, 3), end: Coord::new(0, 5) };
        let line_two = TileLine { start: Coord::new(0, 4), end: Coord::new(0, 8) };
        let line_two_half =  TileLine { start: Coord::new(0, 20), end: Coord::new(0, 23)};
 
        let line_three = TileLine { start: Coord::new(3, 0), end: Coord::new(5, 0) };
        let line_four = TileLine { start: Coord::new(4, 0), end: Coord::new(8, 0) };
        let line_five = TileLine { start: Coord::new(0, 20), end: Coord::new(0, 35)};

        let line_vec = vec![line_zero, line_one, line_two, line_two_half, line_three, line_four, line_five];

        let merged = TileLine::merge_all_tiles(line_vec);
        let expected = vec![TileLine { start: Coord { x: 3, y: 0 }, end: Coord { x: 8, y: 0 } },
                            TileLine { start: Coord { x: 0, y: 16 }, end: Coord { x: 0, y: 35 } },
                            TileLine { start: Coord { x: 0, y: 3 }, end: Coord { x: 0, y: 8 } }];

        assert_eq!(merged, expected);
    }
}
