use crate::util::parsers::read_line_ints;

pub fn solve_21_01(raw_input: String) -> (usize, usize) {
    let input = read_line_ints(&raw_input);
    (get_increments_by(&input, 1), get_increments_by(&input, 3))
}

fn get_increments_by(input: &[i32], by: usize) -> usize {
    let mut increment = 0;
    for i in 0..(input.len() - by) {
        if input[i] < input[i + by] {
            increment += 1
        }
    }
    increment
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::io_helpers::read_input_from_resources;

    #[test]
    fn test_result_21_01() {
        let (year, day) = (2021, 1);
        let expected_result = (1139, 1103);

        let raw_input = read_input_from_resources(year, day, false);
        assert_eq!(solve_21_01(raw_input), expected_result);
    }
}
