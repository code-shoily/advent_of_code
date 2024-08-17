use std::collections::HashSet;

use crate::util::parsers::read_line_ints;

pub fn solve_18_01(raw_input: String) -> (i32, i32) {
    let input = read_line_ints(&raw_input);
    (part_1(&input), part_2(&input))
}

fn part_1(input: &[i32]) -> i32 {
    input.iter().sum()
}

fn part_2(input: &[i32]) -> i32 {
    let mut current_frequency = 0;
    let mut frequencies: HashSet<i32> = HashSet::new();

    for i in input.iter().cycle() {
        current_frequency += *i;
        if frequencies.contains(&current_frequency) {
            return current_frequency;
        }
        frequencies.insert(current_frequency);
    }
    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::io_helpers::read_input_from_resources;

    #[test]
    fn test_result_18_01() {
        let (year, day) = (2018, 1);
        let expected_result = (590, 83445);

        let raw_input = read_input_from_resources(year, day, false);
        assert_eq!(solve_18_01(raw_input), expected_result);
    }
}
