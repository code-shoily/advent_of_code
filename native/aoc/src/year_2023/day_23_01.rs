use itertools::Itertools;

pub fn solve_23_01(raw_input: String) -> (i32, i32) {
    let input: Vec<&str> = raw_input.split('\n').collect();
    (part_1(&input), part_2(&input))
}

fn part_1(input: &[&str]) -> i32 {
    let digits: Vec<&str> = vec!["1", "2", "3", "4", "5", "6", "7", "8", "9"];
    input.iter().map(|line| get_digits(line, &digits)).sum()
}

fn part_2(input: &[&str]) -> i32 {
    let digits: Vec<&str> = vec![
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "1", "2", "3", "4",
        "5", "6", "7", "8", "9",
    ];
    input.iter().map(|line| get_digits(line, &digits)).sum()
}

fn get_digit_int(word: &str, base: &[&str]) -> usize {
    let (pos, _) = base.iter().find_position(|x| **x == word).unwrap();
    (pos % 9) + 1
}

fn get_left_digit(line: &str, digits: &[&str]) -> i32 {
    let mut lim = 10_000;
    let mut left = None;

    for digit in digits {
        match line.find(digit) {
            None => continue,
            Some(value) => {
                if value <= lim {
                    lim = value;
                    left = Some(digit);
                }
            }
        }
    }

    get_digit_int(left.unwrap(), digits) as i32 * 10
}

fn get_right_digit(line: &str, digits: &[&str]) -> i32 {
    let mut lim = 0;
    let mut right = None;

    for digit in digits {
        match line.rfind(digit) {
            None => continue,
            Some(value) => {
                if value >= lim {
                    lim = value;
                    right = Some(digit);
                }
            }
        }
    }

    get_digit_int(right.unwrap(), digits) as i32
}

fn get_digits(line: &str, digits: &[&str]) -> i32 {
    let left = get_left_digit(line, digits);
    let right = get_right_digit(line, digits);

    left + right
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::io_helpers::read_input_from_resources;

    #[test]
    fn test_result_23_01() {
        let (year, day) = (2023, 1);
        let expected_result = (53194, 54249);

        let raw_input = read_input_from_resources(year, day, false);
        assert_eq!(solve_23_01(raw_input), expected_result);
    }
}
