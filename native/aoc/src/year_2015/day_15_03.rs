use std::collections::HashSet;
use std::ops;

pub fn solve_15_03(raw_input: String) -> (usize, usize) {
    let input = process(&raw_input);
    (part_1(&input), part_2(&input))
}

fn process(raw_input: &str) -> Vec<Direction> {
    raw_input.chars().map(Direction::new).collect()
}

fn part_1(input: &[Direction]) -> usize {
    let mut delivery_grid = DeliveryGrid::new();
    for direction in input {
        delivery_grid.deliver(direction);
    }
    delivery_grid.houses.len()
}

fn part_2(input: &[Direction]) -> usize {
    let mut delivery_santa = DeliveryGrid::new();
    let mut delivery_robo = DeliveryGrid::new();

    for (i, direction) in input.iter().enumerate() {
        if i % 2 == 0 {
            delivery_robo.deliver(direction);
        } else {
            delivery_santa.deliver(direction);
        }
    }

    (delivery_robo + delivery_santa).len()
}

#[derive(Clone, Eq, PartialEq, Hash, Copy)]
struct Point {
    x: i32,
    y: i32,
}

impl ops::Add for Point {
    type Output = Point;

    fn add(self, rhs: Self) -> Self::Output {
        Point {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl Point {
    fn origin() -> Self {
        Point { x: 0, y: 0 }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn new(s: char) -> Self {
        match s {
            '^' => Direction::Up,
            'v' => Direction::Down,
            '>' => Direction::Right,
            '<' => Direction::Left,
            _ => unreachable!(),
        }
    }
}

struct DeliveryGrid {
    position: Point,
    houses: HashSet<Point>,
}

impl ops::Add for DeliveryGrid {
    type Output = HashSet<Point>;

    fn add(self, rhs: Self) -> Self::Output {
        self.houses.union(&rhs.houses).copied().collect()
    }
}

impl DeliveryGrid {
    fn new() -> Self {
        DeliveryGrid {
            position: Point::origin(),
            houses: HashSet::from_iter([Point::origin()]),
        }
    }

    fn deliver(&mut self, direction: &Direction) -> bool {
        let point = self.position;

        self.position = match direction {
            Direction::Up => point + Point { x: 0, y: -1 },
            Direction::Down => point + Point { x: 0, y: 1 },
            Direction::Left => point + Point { x: -1, y: 0 },
            Direction::Right => point + Point { x: 1, y: 0 },
        };
        self.houses.insert(self.position)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::io_helpers::read_input_from_resources;

    const YEAR: i16 = 2015;
    const DAY: i8 = 3;

    #[test]
    fn test_process() {
        let given = ">>^<v";
        let expected = vec![
            Direction::Right,
            Direction::Right,
            Direction::Up,
            Direction::Left,
            Direction::Down,
        ];

        assert_eq!(process(given), expected);
    }

    #[test]
    fn test_solution() {
        let given = read_input_from_resources(YEAR, DAY, false);
        let expected = (2081, 2341);

        assert_eq!(solve_15_03(given), expected);
    }
}
