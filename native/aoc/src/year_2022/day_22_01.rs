use crate::util::parsers::read_line_ints;
use itertools::Itertools;
pub fn solve_22_01(raw_input: String) -> (i32, i32) {
    let input = process(raw_input);
    (input[0], input[0..3].iter().sum())
}

fn process(raw_input: String) -> Vec<i32> {
    raw_input
        .split("\n\n")
        .map(|calories| read_line_ints(calories).into_iter().sum())
        .sorted()
        .rev()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::io_helpers::read_input_from_resources;

    #[test]
    fn test_result_22_01() {
        let (year, day) = (2022, 1);
        let expected_result = (70720, 207148);

        let raw_input = read_input_from_resources(year, day, false);
        assert_eq!(solve_22_01(raw_input), expected_result);
    }
}
