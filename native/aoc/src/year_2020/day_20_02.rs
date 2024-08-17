pub fn solve_20_02(raw_input: String) -> (usize, usize) {
    let input = process(&raw_input);
    (part_1(&input), part_2(&input))
}

fn process(raw_input: &str) -> Vec<Policy> {
    raw_input.split('\n').map(Policy::new).collect()
}

#[derive(Debug, Eq, PartialEq)]
struct Policy {
    start: usize,
    end: usize,
    ch: char,
    passwd: String,
}

impl Policy {
    fn new(line: &str) -> Self {
        let tokens: Vec<&str> = line.split(' ').collect();
        let limits: Vec<usize> = tokens[0]
            .split('-')
            .map(|i| i.parse::<usize>().unwrap())
            .collect();
        let ch: char = tokens[1].chars().next().unwrap();
        let passwd = tokens[2].into();

        Policy {
            start: limits[0],
            end: limits[1],
            ch,
            passwd,
        }
    }

    fn is_valid_1(&self) -> bool {
        (self.start..=self.end).contains(&self.passwd.chars().filter(|&c| c == self.ch).count())
    }

    fn is_valid_2(&self) -> bool {
        let pos_1 = self.passwd.as_bytes()[self.start - 1] == self.ch as u8;
        let pos_2 = self.passwd.as_bytes()[self.end - 1] == self.ch as u8;

        (pos_1 || pos_2) && !(pos_1 && pos_2)
    }
}

fn part_1(input: &[Policy]) -> usize {
    input.iter().filter(|policy| policy.is_valid_1()).count()
}

fn part_2(input: &[Policy]) -> usize {
    input.iter().filter(|policy| policy.is_valid_2()).count()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::io_helpers::read_input_from_resources;

    const YEAR: i16 = 2020;
    const DAY: i8 = 2;

    #[test]
    fn test_process() {
        let given = "1-3 a: password\n4-9 c: character";
        let policy_1 = Policy {
            start: 1,
            end: 3,
            ch: 'a',
            passwd: String::from("password"),
        };
        let policy_2 = Policy {
            start: 4,
            end: 9,
            ch: 'c',
            passwd: String::from("character"),
        };
        let expected = vec![policy_1, policy_2];

        assert_eq!(process(given), expected);
    }

    #[test]
    fn test_policy() {
        let line = "1-3 a: password";
        let policy = Policy {
            start: 1,
            end: 3,
            ch: 'a',
            passwd: String::from("password"),
        };
        assert_eq!(Policy::new(line), policy)
    }

    #[test]
    fn test_solution() {
        let given = read_input_from_resources(YEAR, DAY, false);
        let expected = (607, 321);

        assert_eq!(solve_20_02(given), expected);
    }
}
