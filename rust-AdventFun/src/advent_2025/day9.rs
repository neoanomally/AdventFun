use std::{cmp::Ordering, collections::{HashMap, HashSet, VecDeque}, hash::Hash}; 

use crate::advent_io::advent_file::read2025;


// TODO: 
// [1] Create all the edges
// [2] Create Rectangles
// [3] Check to see if the Rectangle passes any edges
// [4] If it doesn't pass any edges; then 
//      We can do this by checking if all edges are on the same side of the line. 
//      We can even do this by vertical and horizontal edges. 
//      _______________|_______________
//      |              |              |
//      |              |              |
//      ---------------|---------------
//                     |
//  In the above exxample we can see that the line goes through the center so the left edge and the
//  right edge are on different sides of the line. 
//
// To quickly find the largest rectangle. For each pair calculate the area. 
// Go from largest area downwards until we find a rectangle that doesn't intersect. 
#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
enum CoordDir {
    Left,
    Right,
    Up,
    Down,
    OnTheLine,
    NotWithin
}


#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
struct Coord {
    x: u64,
    y: u64
}

impl Coord {
    fn new(x: u64, y: u64) -> Coord {
        Coord { x, y }
    }
}

#[allow(dead_code)]
#[derive(Debug)]
struct Polygon {
    lines: Vec<TileLine>,
    cache: HashMap<Coord, bool>
}

impl Polygon {
    #[allow(dead_code)]
    fn new(lines: Vec<TileLine>) -> Polygon {
        Polygon { lines, cache: HashMap::new() }
    }

    // I need to check EVERY wall and make sure that a point is surrounded by a wall in each
    // direction. We are going to start the easiesst assumption first in that this polygon does not
    // have any weird shapes such that there are weird holes / edge cases. That EVRY rectange will 
    // Check every edge of the polygon nd see if the start and end points of those lines are inside
    // the polygon. I'm also going to keep track of how many edges are to the left, right up or
    // down to the polygon. THis assumes that the polygon doesn't have an instance where the line
    // of the rectangle doesn't get broken in half such that the polygon makes a curve. Example
    // below while this is in the polygon i"m not checking for this case to begin with. If I have
    // to handle this situation I'm going to check the CLOSEST lines first then transforming the
    // lin after it hits the first wall
    // going to check coords first but there is a situation where a coord isnt' good enough 
    //      ________________
    //           ____________
    //   ________|
    #[allow(dead_code,unused_variables)]
    fn is_coord_inside(&mut self, coord: &Coord) -> bool {
        if self.cache.contains_key(coord) {
            return *self.cache.get(coord).unwrap();
        }
        let mut left = 0;
        let mut right = 0;
        let mut down = 0; 
        let mut up = 0;
        let mut not_within = 0;
        let mut on_the_line = 0;
        
        for edge in self.lines.iter() {
            // TODO Coord_DIR should return a value as well? 
            // TODO: Coord DIR should just increment when on the same vertical or horizontal for
            // the line.  
            let dir = edge.coord_dir(coord);

            match dir {
                CoordDir::Left => left += 1,
                CoordDir::Right => right += 1,
                CoordDir::Up => up += 1,
                CoordDir::Down => down += 1,
                CoordDir::NotWithin =>  not_within += 1,
                CoordDir::OnTheLine => on_the_line += 1
            }

        }

        let is_on_the_line = on_the_line > 0;
        // For the love of god. HOW DO I handle; when a point passses but its on another line
        //  ONE WAY is when I find a line that I'm above or below I subtract one from that
        //  direction
        let mut set: HashSet<Coord> = HashSet::new();
        set.insert(Coord::new(9, 3));
        set.insert(Coord::new(2, 3));
        set.insert(Coord::new(2, 5));
        set.insert(Coord::new(9, 5));
        let all_sides_found = up % 2 != 0 && down % 2 != 0 && left % 2 != 0 && right % 2 != 0;
        // if set.contains(coord) {
            // println!("Coord: {:?} All sides: up= {} down= {} left= {} right= {} Out= {} In= {}", coord, up, down, left, right, not_within, on_the_line);
        // }

        let result = is_on_the_line || all_sides_found;
        self.cache.insert(coord.clone(), result);
        result
    }

    fn find_red_tiles_max_area_within(&mut self, coords: &[Coord]) -> u64 {
        let mut lines: HashSet<TileLine> = HashSet::new();  
        for i in 0..coords.len() {
            for j in i+1..coords.len() {
                let first_coord = coords.get(i).unwrap();
                let second_coord = coords.get(j).unwrap();

                if first_coord.x == second_coord.x ||  first_coord.y == second_coord.y
                  && first_coord != second_coord {
                    lines.insert(
                        TileLine::new(
                            std::cmp::min(first_coord, second_coord).clone(),
                            std::cmp::max(first_coord, second_coord).clone()
                        ));
                }
            }
        }

        let line_counts = lines.iter().flat_map(|line| vec![line.start.clone(), line.end.clone()])
            .fold(HashMap::<Coord, u32>::new(), |mut acc, coord| {
                acc.entry(coord)
                    .and_modify(|f| *f += 1)
                    .or_insert(1);
                acc
            });
        // Need to fix the way I construct the connected circle. 
        let filtered_lines: Vec<TileLine> = lines.into_iter()
            .filter(|line| *line_counts.get(&line.start).unwrap() == 2 && *line_counts.get(&line.end).unwrap() == 2)
            .collect();
        println!("Number of lines: {}", filtered_lines.len());


        let mut areas: Vec<(Coord, Coord, u64)> = Vec::new();
        for i in 0..coords.len() {
            for j in i+1..coords.len() {
                let first_coord = coords.get(i).unwrap();
                let second_coord = coords.get(j).unwrap();

                let height =    first_coord.y.abs_diff(second_coord.y) + 1;
                let width =     first_coord.x.abs_diff(second_coord.x) + 1;

                let area = height * width;
                areas.push((first_coord.clone(), second_coord.clone(), area));
            }
        }
    // check to see if any LINE intersects with the rectangle. 
        areas.sort_by(|a, b| b.2.cmp(&a.2));
        let first_area_inside = areas.iter()
            .find(|(a, b, _)| {
                filtered_lines.iter().all(|line| {
                    let is_left =   a.x.max(b.x) <= line.start.x.min(line.end.x);
                    let is_right =  a.x.min(b.x) >= line.start.x.max(line.end.x);
                    let is_up =     a.y.max(b.y) <= line.start.y.min(line.end.y);
                    let is_down =   a.y.min(b.y) >= line.start.y.max(line.end.y);

                    is_up || is_down || is_left || is_right
                })
            });

        first_area_inside.expect("Should find at least one rectangle that meets these conditions").2
    }

    fn num_edges(&self) -> usize {
        self.lines.len()
    }
}

struct GreenTileLines {
    lines: Vec<TileLine>
}


impl GreenTileLines {
    // Ended up not using this  but very proud of how this method came out and worked. It actually
    // does a binary search when finding if a line is within another line
    // Might use it if I really need to.
    #[allow(dead_code)]
    fn binary_search_is_within(&self, line: &TileLine) -> bool {
        let mut left = 0; 
        let mut right = self.lines.len();
        let is_vertical = line.is_vertical();

        while left < right {
            let mid = left + ((right - left) / 2);
            let mid_entry = self.lines.get(mid).unwrap();

            let ordering_comp = if is_vertical {
                line.cmp(mid_entry)
            } else {
                line.cmp_horizontal(mid_entry)
            };

            if line.is_fully_within(mid_entry) {
                return true
            } else if ordering_comp == Ordering::Less {
                left = mid + 1;
            } else  {
                right = mid;
            }
        }

        false
    }

    // Really these are creating lines to make sure any rectangle that we create are within existing
    // lines. So once we have the set of lines, we can see if any given rectangle lines are within
    // green lines. 
    // 1. Find all lines
    // 2. Merge overlapping lines
    // 3. index each line into two hashmaps: Vertical Lookups and Horizontal lookupsjjj
    fn from_coords(input: &[Coord]) -> GreenTileLines {
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

        println!("The number of lines: {}", deque.len());
        deque.into()
    }

    fn get_polygons(&self) -> Vec<Polygon> {
        GreenTileLines::create_polygons(&self.lines)
    }

    fn create_polygons(tiles: &[TileLine]) -> Vec<Polygon> {
        GreenTileLines::connect_lines(tiles)
            .into_iter()
            .filter(GreenTileLines::is_polygon)
            .map(|mut cluster| {
                cluster.sort();
                Polygon::new(cluster)
            })
            .collect()
    }

    fn connect_lines(tiles: &[TileLine]) -> Vec<Vec<TileLine>> {
        let mut line_index: HashMap<TileLine, usize> = HashMap::new();
        let mut clusters: HashMap<usize, Vec<TileLine>> = HashMap::new();

        for tidx in 0..tiles.len()  {
            let cloned = tiles.get(tidx).unwrap().clone();
            line_index.insert(cloned.clone(), tidx);
            clusters.insert(tidx, vec![cloned]);
        }

        for i in 0..tiles.len() {
            for j in i+1..tiles.len() {

                let left = tiles.get(i).unwrap();
                let right = tiles.get(j).unwrap();
                let left_idx = *line_index.get(left).unwrap();
                let right_idx = *line_index.get(right).unwrap();

                if left.can_connect(right) && left_idx != right_idx {
                    // What happens if you remove an entry and then it no longer exists. the line
                    // below `cluster.entry(....)....` will fail because it will only modify
                    // entries where the key exists; so you should have some guard around it. 
                    let right_cluster: Vec<TileLine> = clusters.remove(&right_idx).unwrap();

                    for i in 0..right_cluster.len() {
                        let current_line = right_cluster.get(i).unwrap();
                        line_index.insert(current_line.clone(), left_idx);
                    }

                    // INTERESTING. if you try to do this on it's self (e.g. when leftidx ==
                    // rightidx bad stuff happens.. You need to use and_modify with or insert.
                    clusters.entry(left_idx).and_modify(|left| left.extend(right_cluster));
                }   
            }
        }

        clusters.into_values().collect()
    }


    #[allow(clippy::ptr_arg)]
    fn is_polygon(cluster: &Vec<TileLine>) -> bool {
        let mut connected_polygon: Vec<TileLine> = Vec::new();
        if cluster.len() < 3 {
            return false;
        }

        let mut cloned = cluster.clone().to_vec();
        let starter_line = cloned.remove(0);
        let starting_point: Coord = starter_line.start.clone();
        let mut did_pull = true;
        let mut end_point = starter_line.end.clone();
        connected_polygon.push(starter_line);


        while !cloned.is_empty() && did_pull {
            did_pull = false;

            for i in 0..cloned.len() {
                let next: TileLine = cloned.get(i).unwrap().clone();

                if next.start == end_point {
                    end_point = next.end.clone();
                    connected_polygon.push(cloned.remove(i));
                    did_pull = true;

                    if next.end == starting_point { return true; } else { break; }

                } else if next.end == end_point {
                    end_point = next.start.clone();
                    connected_polygon.push(cloned.remove(i));
                    did_pull = true;
                    
                    if next.start == starting_point { return true; } else {break; }
                }
            }
        }

        false
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
struct TileLine {
    start: Coord, 
    end: Coord
}

impl TileLine {

    fn cmp_horizontal(&self, other: &TileLine) -> Ordering {
        assert!(!self.is_vertical());
        self.start.y.cmp(&other.start.y)
            .then(self.end.y.cmp(&other.end.y))
            .then(self.start.x.cmp(&other.start.x))
            .then(self.end.x.cmp(&other.end.x))
    }


    fn new(left: Coord, right: Coord) -> TileLine {
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
            let updated_start = Coord::new(self.start.x, std::cmp::min(self.start.y, other.start.y));
            let updated_end = Coord::new(self.start.x, std::cmp::max(self.end.y, other.end.y) );

            Some(TileLine{ start: updated_start, end: updated_end })
        } else if self.has_overlapping_vertical(&other) {
            let updated_start = Coord::new(std::cmp::min(self.start.x, other.start.x), self.start.y);
            let updated_end = Coord::new(std::cmp::max(self.end.x, other.end.x), self.start.y );

            Some(TileLine { start: updated_start, end: updated_end })
        } else {
            None
        }
    }

    fn can_connect(&self, other: &TileLine) -> bool {
        self.start == other.start
            || self.start == other.end
            || self.end == other.start
            || self.end == other.end
    }

    fn is_vertical(&self) -> bool {
        self.start.x == self.end.x
    }

    fn has_overlapping_horizontal(&self, other: &TileLine) -> bool {
        let all_same_x = self.start.x == other.start.x && self.start.x == other.end.x && self.start.x == self.end.x;
        all_same_x && ((self.start.y >= other.start.y &&  self.end.y >= other.end.y) 
            || (self.end.y >= other.start.y && self.end.y <= other.end.y))
    }

    #[allow(clippy::nonminimal_bool)]
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

    fn coord_dir(&self, coord: &Coord) -> CoordDir {
        if self.is_vertical() {

            let max_y = std::cmp::max(self.start.y, self.end.y);
            let min_y = std::cmp::min(self.start.y, self.end.y);
            let is_within = coord.y <= max_y && coord.y >= min_y;
            let is_on_x = coord.x == self.start.x;
            let is_on_the_line = is_on_x && is_within;

            if is_on_the_line {
                CoordDir::OnTheLine
            } else if is_on_x && coord.y < min_y {
                CoordDir::Up
            } else if is_on_x && coord.y > max_y {
                CoordDir::Down
            } else if is_within && coord.x < self.start.x {
                CoordDir::Left
            } else if is_within && coord.x > self.start.x {
                CoordDir::Right
            } else {
                CoordDir::NotWithin
            }
            
        } else {
            let max_x = std::cmp::max(self.start.x, self.end.x);
            let min_x = std::cmp::min(self.start.x, self.end.x);
            let is_within = coord.x <= max_x && coord.x >= min_x;
            let is_on_y = coord.y == self.start.y;
            let is_on_the_line = is_on_y && is_within;

            if is_on_the_line {
                CoordDir::OnTheLine
            } else if is_on_y && coord.x < min_x {
                CoordDir::Left
            } else if is_on_y && coord.x > max_x {
                CoordDir::Right
            } else if is_within && coord.y < self.start.y {
                CoordDir::Up
            } else if is_within && coord.y > self.start.y {
                CoordDir::Down
            } else {
                CoordDir::NotWithin
            }
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
    let red_tiles = parse_input(input);
    let max_area = find_max_rectangle_area(&red_tiles);

    let green_tiles_lines = GreenTileLines::from_coords(&red_tiles);

    let mut polygons = green_tiles_lines.get_polygons();
    assert_eq!(polygons.len(), 1);
    let mut polygon = polygons.remove(0);
    println!("Polygon has {} edges", polygon.num_edges());
    println!("The max area for Part 1: {max_area}");

    let max_gree_area_in_polygon = polygon.find_red_tiles_max_area_within(&red_tiles);

    println!("Max green area in polygon: {max_gree_area_in_polygon}");

}

fn find_max_rectangle_area(coords: &[Coord]) -> u64 {
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
    use std::{collections::HashSet, vec};

    use super::*;

    #[test]
    fn test_parse_input() {
        let input: Vec<String> = ["7,1", "11,1", "9,7"].iter().map(|f| f.to_string()).collect();
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
    fn test_is_vertical() {
        let vertical_line = TileLine::new(Coord::new(3, 1), Coord::new(3, 7));
        let horizontal_line = TileLine::new(Coord::new(1, 3), Coord::new(5, 3));


        assert!(vertical_line.is_vertical());
        assert!(!horizontal_line.is_vertical());
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

        let green_tiles: HashSet<TileLine> = GreenTileLines::from_coords(&coords).lines.into_iter().collect();

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

    #[test]
    fn test_tiles_can_connect() {
        let first = TileLine { start: Coord { x: 9, y: 7 }, end: Coord { x: 11, y: 7 } };
        let second = TileLine { start: Coord { x: 7, y: 1 }, end: Coord { x: 7, y: 3 } };
        let third = TileLine { start: Coord { x: 2, y: 3 }, end: Coord { x: 7, y: 3 } };
        let fourth = TileLine { start: Coord { x: 9, y: 7 }, end: Coord { x: 9, y: 3 } };

        let should_not_one = first.can_connect(&second);
        let should_not_two = first.can_connect(&third);
        let should_connect_one = second.can_connect(&third);
        let should_connect_two = first.can_connect(&fourth);

        assert!(!should_not_one);
        assert!(!should_not_two);
        assert!(should_connect_one);
        assert!(should_connect_two);
    }

    #[test]
    fn test_connect_all_tiles() {
        // ...o......o...  3, 0       10, 0 
        // ..............
        // ...o......o...  3, 2       10, 2
        // ..............
        let lines = vec![TileLine { start: Coord { x: 9, y: 7 }, end: Coord { x: 11, y: 7 } },
            TileLine { start: Coord { x: 7, y: 1 }, end: Coord { x: 7, y: 3 } },
            TileLine { start: Coord { x: 2, y: 3 }, end: Coord { x: 7, y: 3 } },
            TileLine { start: Coord { x: 9, y: 7 }, end: Coord { x: 9, y: 3 } }];

        let lines_two = vec![
            TileLine { start: Coord { x: 3, y: 0 }, end: Coord { x: 3, y: 2 } },
            TileLine { start: Coord { x: 3, y: 2 }, end: Coord { x: 10, y: 2 } },
            TileLine { start: Coord { x: 10, y: 2 }, end: Coord { x: 10, y: 0 } },
            TileLine { start: Coord { x: 10, y: 0 }, end: Coord { x: 3, y: 0 } }
        ];

        let mut connected = GreenTileLines::connect_lines(&lines);
        
        let mut expected = vec![
            vec![TileLine { start: Coord { x: 9, y: 7 }, end: Coord { x: 11, y: 7 } },
            TileLine { start: Coord { x: 9, y: 7 }, end: Coord { x: 9, y: 3 } }],
            vec![TileLine { start: Coord { x: 7, y: 1 }, end: Coord { x: 7, y: 3 } },
            TileLine { start: Coord { x: 2, y: 3 }, end: Coord { x: 7, y: 3 } }]]; 

        connected.sort();
        expected.sort();

        assert_eq!(connected, expected);

        let is_polygon = GreenTileLines::is_polygon(&lines_two);
        let polygons = GreenTileLines::create_polygons(&lines); // Add some full polygon
        let polygons_two = GreenTileLines::create_polygons(&lines_two);
        assert!(is_polygon);
        assert_eq!(polygons.len(), 0);
        assert_eq!(polygons_two.len(), 1)
    }

    #[test]
    fn test_is_polygon() {
        let lines = vec![TileLine { start: Coord { x: 9, y: 7 }, end: Coord { x: 11, y: 7 } },
            TileLine { start: Coord { x: 11, y: 3 }, end: Coord { x: 11, y: 7 } },
            TileLine { start: Coord { x: 7, y: 3 }, end: Coord { x: 11, y: 3 } },
            TileLine { start: Coord { x: 7, y: 3 }, end: Coord { x: 9, y: 7 } }];

        let mut lines_not_poly = lines.clone();
        lines_not_poly.remove(3);
        lines_not_poly.push(TileLine::new(Coord::new(8, 1), Coord::new(3, 1)));

       let should_be_polygon = GreenTileLines::is_polygon(&lines); 
       let should_not_be_polygon = GreenTileLines::is_polygon(&lines_not_poly);

       assert!(should_be_polygon);
       assert!(!should_not_be_polygon);
    }

    #[test]
    fn test_polygon_coords() {
        let lines = vec![
            TileLine { start: Coord { x: 3, y: 0 }, end: Coord { x: 3, y: 2 } },
            TileLine { start: Coord { x: 3, y: 2 }, end: Coord { x: 10, y: 2 } },
            TileLine { start: Coord { x: 10, y: 2 }, end: Coord { x: 10, y: 0 } },
            TileLine { start: Coord { x: 10, y: 0 }, end: Coord { x: 3, y: 0 } }
        ];

        let mut polygons = GreenTileLines::create_polygons(&lines);
        let mut polygon = polygons.remove(0);
        let is_inside = polygon.is_coord_inside(&Coord::new(5, 1));
        let is_outside = polygon.is_coord_inside(&Coord::new(11, 11));

        assert!(is_inside);
        assert!(!is_outside);
    }


    #[test]
    fn test_is_coord_wiithin() {
        let vertical_line = TileLine::new(Coord::new(3, 8), Coord::new(3, 1));
        let left = Coord::new(1, 6);
        let right = Coord::new(5, 5);

        let horizontal_line = TileLine::new(Coord::new(1, 6), Coord::new(9, 6));
        let up = Coord::new(1, 5);
        let down = Coord::new(5, 9);
        let test_line_a = TileLine { start: Coord { x: 2, y: 3 }, end: Coord { x: 2, y: 5 } };
        let test_first_coord_a = Coord::new(9, 5);
        let test_second_coord_a = Coord::new(2, 3);

        let not_in = Coord::new(26, 87);

        assert_eq!(vertical_line.coord_dir(&left), CoordDir::Left);
        assert_eq!(vertical_line.coord_dir(&right), CoordDir::Right);
        assert_eq!(horizontal_line.coord_dir(&up), CoordDir::Up);
        assert_eq!(horizontal_line.coord_dir(&down), CoordDir::Down);

        assert_eq!(horizontal_line.coord_dir(&not_in), CoordDir::NotWithin);
        assert_eq!(vertical_line.coord_dir(&not_in), CoordDir::NotWithin);

        assert_eq!(test_line_a.coord_dir(&test_first_coord_a), CoordDir::Right);
        assert_eq!(test_line_a.coord_dir(&test_second_coord_a), CoordDir::OnTheLine);
    }
}
