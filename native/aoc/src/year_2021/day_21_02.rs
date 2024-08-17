pub fn solve_21_02(raw_input: String) -> (i32, i32) {
    let input = process(&raw_input);
    (part_1(&input), part_2(&input))
}

fn part_1(input: &[Instruction]) -> i32 {
    let mut sub = Submarine::new();
    for instruction in input {
        sub.follow_1(instruction);
    }
    sub.value()
}

fn part_2(input: &[Instruction]) -> i32 {
    let mut sub = Submarine::new();
    for instruction in input {
        sub.follow_2(instruction);
    }
    sub.value()
}

fn process(raw_input: &str) -> Vec<Instruction> {
    raw_input.split('\n').map(Instruction::from_str).collect()
}

#[derive(Debug, Eq, PartialEq)]
enum Direction {
    Forward,
    Up,
    Down,
}

#[derive(Debug, Eq, PartialEq)]
struct Instruction {
    direction: Direction,
    steps: i32,
}

impl Instruction {
    fn from_str(line: &str) -> Self {
        let tokens: Vec<&str> = line.split(' ').collect();
        let direction: Direction = match tokens[0] {
            "forward" => Direction::Forward,
            "up" => Direction::Up,
            "down" => Direction::Down,
            _ => unreachable!(),
        };
        let steps = tokens[1].parse::<i32>().unwrap();

        Instruction { direction, steps }
    }
}

struct Submarine {
    aim: i32,
    horizontal_position: i32,
    depth: i32,
}

impl Submarine {
    fn new() -> Self {
        Submarine {
            aim: 0,
            horizontal_position: 0,
            depth: 0,
        }
    }

    fn follow_1(&mut self, instruction: &Instruction) {
        let Instruction { direction, steps } = instruction;
        match direction {
            Direction::Forward => self.horizontal_position += steps,
            Direction::Up => self.depth -= steps,
            Direction::Down => self.depth += steps,
        }
    }

    fn follow_2(&mut self, instruction: &Instruction) {
        let Instruction { direction, steps } = instruction;
        match direction {
            Direction::Forward => {
                self.horizontal_position += steps;
                self.depth += self.aim * steps;
            }
            Direction::Up => self.aim -= steps,
            Direction::Down => self.aim += steps,
        }
    }

    fn value(self) -> i32 {
        self.horizontal_position * self.depth
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::io_helpers::read_input_from_resources;

    #[test]
    fn test_process() {
        let given = "forward 10\nup 9\ndown 0\nforward 20";
        let expected = vec![
            Instruction {
                direction: Direction::Forward,
                steps: 10,
            },
            Instruction {
                direction: Direction::Up,
                steps: 9,
            },
            Instruction {
                direction: Direction::Down,
                steps: 0,
            },
            Instruction {
                direction: Direction::Forward,
                steps: 20,
            },
        ];

        assert_eq!(process(given), expected);
    }

    #[test]
    fn test_solution() {
        let raw_input = read_input_from_resources(2021, 2, false);
        let output = (1660158, 1604592846);

        assert_eq!(solve_21_02(raw_input), output);
    }
}
