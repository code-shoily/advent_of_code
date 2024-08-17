use std::collections::HashSet;

pub fn solve_16_01(raw_input: String) -> (i32, i32) {
    let input = process_input(&raw_input);

    let mut position = Position::new();
    input.iter().for_each(|instruction| {
        position.step(instruction);
    });

    let part_1 = position.location.manhattan_distance();
    let part_2 = position.repeat.unwrap().manhattan_distance();

    (part_1, part_2)
}

fn process_input(raw_input: &str) -> Vec<(Direction, i32)> {
    raw_input
        .trim()
        .split(", ")
        .map(|i| {
            let direction = match &i[0..=0] {
                "R" => Direction::Right,
                "L" => Direction::Left,
                value => panic!("Invalid instruction {value}"),
            };
            let steps: i32 = i[1..].parse::<i32>().unwrap();

            (direction, steps)
        })
        .collect()
}

enum Facing {
    North,
    East,
    South,
    West,
}

enum Direction {
    Left,
    Right,
}

#[derive(Debug)]
struct Point {
    pub x: i32,
    pub y: i32,
}

impl Point {
    pub fn manhattan_distance(&self) -> i32 {
        self.x.abs() + self.y.abs()
    }
}

struct Position {
    facing: Facing,
    location: Point,
    visits: HashSet<(i32, i32)>,
    repeat: Option<Point>,
}

impl Position {
    pub fn new() -> Position {
        Position {
            facing: Facing::North,
            location: Point { x: 0, y: 0 },
            visits: vec![(0, 0)].into_iter().collect(),
            repeat: Option::None,
        }
    }

    pub fn step(&mut self, instruction: &(Direction, i32)) {
        let (direction, steps) = instruction;

        match (direction, &self.facing) {
            (Direction::Left, Facing::North) => self.move_to((-1, 0), *steps),
            (Direction::Left, Facing::East) => self.move_to((0, 1), *steps),
            (Direction::Left, Facing::South) => self.move_to((1, 0), *steps),
            (Direction::Left, Facing::West) => self.move_to((0, -1), *steps),
            (Direction::Right, Facing::North) => self.move_to((1, 0), *steps),
            (Direction::Right, Facing::East) => self.move_to((0, -1), *steps),
            (Direction::Right, Facing::South) => self.move_to((-1, 0), *steps),
            (Direction::Right, Facing::West) => self.move_to((0, 1), *steps),
        }
    }

    fn move_to(&mut self, delta: (i32, i32), steps: i32) {
        let (x_diff, y_diff) = delta;

        self.facing = Self::facing_for_delta(delta);

        for _ in 0..steps {
            let Point { x, y } = self.location;
            self.location = match x_diff {
                0 => Point { x, y: y + y_diff },
                _ => Point { x: x + x_diff, y },
            };

            self.update_visits();
        }
    }

    fn facing_for_delta(delta: (i32, i32)) -> Facing {
        match delta {
            (-1, 0) => Facing::West,
            (0, 1) => Facing::North,
            (1, 0) => Facing::East,
            (0, -1) => Facing::South,
            _ => unreachable!(),
        }
    }

    fn update_visits(&mut self) {
        if self.repeat.is_none() {
            let Point { x, y } = self.location;
            if self.visits.contains(&(x, y)) {
                self.repeat = Some(Point { x, y });
            }
            self.visits.insert((x, y));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::io_helpers::read_input_from_resources;

    #[test]
    fn test_result_16_01() {
        let (year, day) = (2016, 1);
        let expected_result = (253, 126);

        let raw_input = read_input_from_resources(year, day, false);
        assert_eq!(solve_16_01(raw_input), expected_result);
    }
}
