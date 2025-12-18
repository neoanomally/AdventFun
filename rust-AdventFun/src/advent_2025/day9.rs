use std::{cmp::Ordering, collections::VecDeque}; 

use crate::advent_io::advent_file::read2025;


// TODO: 
// [x] 1. Create all the green lines by going through all the red tiles. We should split these by
//    horizontal and veritcal lines. -- Actually this may not be necessary given we have them
//    sorted. 
// [X] 2. Merge all the green lines through the function that has already been created.
// [X] 3. Binary search to find if a line exists in vector for either the horizontal or vertical lines
// [ ] 4. Create a function that lets us run through each red to red corner
//        4a. For each one generate for lines that represent the rectangle.
//        4b. Check if all four of those lines exist in our dictionary. 
//        4c. If so check the area of each of those rectangles. 


#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
struct Coord {
    x: u64,
    y: u64
}

impl Coord {
    fn new(x: u64, y: u64) -> Coord {
        Coord { x: x, y: y }
    }
}


struct GreenTileLines {
    lines: Vec<TileLine>
}


impl GreenTileLines {
    fn binary_search_is_within(&self, line: &TileLine) -> bool {
        let mut left = 0; 
        let mut right = self.lines.len();
        let is_vertical = line.is_vertical();

        while left < right {
            let mid = left + ((right - left) / 2);
            let mid_entry = self.lines.get(mid).unwrap();

            let ordering_comp = if is_vertical {
                line.cmp_vertical(&mid_entry)
            } else {
                line.cmp(&mid_entry)
            };

            if line.is_fully_within(&mid_entry) {
                return true
            } else if ordering_comp == Ordering::Less {
                left = mid + 1;
            } else  {
                right = mid;
            }
        }

        return false;
    }

    // Really these are creating lines to make sure any rectangle that we create are within existing
    // lines. So once we have the set of lines, we can see if any given rectangle lines are within
    // green lines. 
    // 1. Find all lines
    // 2. Merge overlapping lines
    // 3. index each line into two hashmaps: Vertical Lookups and Horizontal lookupsjjj
    fn from_coords(input: Vec<Coord>) -> GreenTileLines {
        let mut green_tiles: Vec<TileLine> = Vec::new();

        for i in 0..input.len() {
            for j in i+1..input.len() {
                let left = input.get(i).unwrap();
                let right = input.get(j).unwrap();

                if left.x == right.x || left.y == right.y {
                    let tile_line = TileLine::new(left.clone(), right.clone());
                    green_tiles.push(tile_line);
                }
            }
        }

        GreenTileLines {
            lines: Self::merge_all_tiles(green_tiles)
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
struct TileLine {
    start: Coord, 
    end: Coord
}

impl TileLine {
    // we should validate that it is verticle.
    fn cmp_vertical(&self, other: &TileLine) -> Ordering {
        assert!(self.is_vertical());
        self.start.y.cmp(&other.start.y)
            .then(self.end.y.cmp(&other.end.y))
            .then(self.start.x.cmp(&other.start.x))
            .then(self.end.x.cmp(&other.end.x))
    }


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

    fn is_vertical(&self) -> bool {
        self.start.y == self.end.y
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
    let max_area = find_max_rectangle_area(&coords);

    let green_tiles_lines = GreenTileLines::from_coords(coords);

    println!("The max area for Part 1: {}", max_area);
}

fn find_max_green_tiles_rectangle(coords: &Vec<Coord>, green_tile_lines: &GreenTileLines) -> u64 {
    todo!("Will implement eventually")
}

fn find_max_rectangle_area(coords: &Vec<Coord>) -> u64 {
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
    use std::collections::HashSet;

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

        let max_opposing = find_max_rectangle_area(&coords);
        assert_eq!(max_opposing, 50);
    }
    
    #[test]
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

        let merged = GreenTileLines::merge_all_tiles(line_vec);
        let expected = vec![TileLine { start: Coord { x: 3, y: 0 }, end: Coord { x: 8, y: 0 } },
                            TileLine { start: Coord { x: 0, y: 16 }, end: Coord { x: 0, y: 35 } },
                            TileLine { start: Coord { x: 0, y: 3 }, end: Coord { x: 0, y: 8 } }];

        assert_eq!(merged, expected);
    }

    #[test]
    fn test_create_green_tiles() {
        let test_input = read2025("test_day9_input");
        let coords = parse_input(test_input);

        let green_tiles: HashSet<TileLine> = GreenTileLines::from_coords(coords).lines.into_iter().collect();

        let expected_tile_lines = vec![
            TileLine::new(Coord::new(7, 1), Coord::new(11, 1)),
            TileLine::new(Coord::new(7, 1), Coord::new(7, 3)),
            TileLine::new(Coord::new(2, 3), Coord::new(2, 5))
        ];


        for expected in expected_tile_lines {
            assert!(green_tiles.contains(&expected));
        }
    }

    #[test]
    fn test_is_fully_within() {
        let line_zero = TileLine { start: Coord::new(0, 16), end: Coord::new(0, 21)};
        let line_one = TileLine { start: Coord::new(0, 3), end: Coord::new(0, 5) };
        let line_two_half =  TileLine { start: Coord::new(0, 18), end: Coord::new(0, 20)};
        let line_three = TileLine { start: Coord::new(1, 18), end: Coord::new(1, 20) };

        let zero_one_not_in = line_zero.is_fully_within(&line_one);
        let two_half_zero_is_in = line_two_half.is_fully_within(&line_zero);
        let line_three_zero_not_in = line_three.is_fully_within(&line_zero);
        let zero_within_itself = line_zero.is_fully_within(&line_zero);

        assert!(!zero_one_not_in);
        assert!(two_half_zero_is_in);
        assert!(!line_three_zero_not_in);
        assert!(zero_within_itself);
    }

    #[test]
    fn test_binary_search_fully_within() {
        let green_tiles = GreenTileLines { lines: vec![TileLine { start: Coord { x: 11, y: 1 }, end: Coord { x: 11, y: 7 } },
                        TileLine { start: Coord { x: 9, y: 7 }, end: Coord { x: 11, y: 7 } },
                        TileLine { start: Coord { x: 7, y: 1 }, end: Coord { x: 7, y: 3 } },
                        TileLine { start: Coord { x: 2, y: 3 }, end: Coord { x: 7, y: 3 } },
                        TileLine { start: Coord { x: 9, y: 5 }, end: Coord { x: 9, y: 7 } },
                        TileLine { start: Coord { x: 2, y: 5 }, end: Coord { x: 9, y: 5 } },
                        TileLine { start: Coord { x: 7, y: 1 }, end: Coord { x: 11, y: 1 } },
                        TileLine { start: Coord { x: 2, y: 3 }, end: Coord { x: 2, y: 5 } }]
        };


        let line_one_inside = TileLine { start: Coord { x: 11, y: 1 }, end: Coord { x: 11, y: 7 } };
        let line_two_way_outside = TileLine { start: Coord { x: 15, y: 1 }, end: Coord { x: 15, y: 7 } };
        let line_within = TileLine { start: Coord { x: 4, y: 5 }, end: Coord { x: 7, y: 5 } };

        let should_be_true_one = green_tiles.binary_search_is_within(&line_one_inside);
        let should_not_be_in = green_tiles.binary_search_is_within(&line_two_way_outside);
        let should_be_true_two = green_tiles.binary_search_is_within(&line_within);

        assert!(!should_not_be_in);
        assert!(should_be_true_one);
        assert!(should_be_true_two);
    }
}
