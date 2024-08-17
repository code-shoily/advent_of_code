pub fn solve_17_01(raw_input: String) -> (i32, i32) {
    let processed_input = process(raw_input);

    (part_1(&processed_input), part_2(&processed_input))
}

fn process(raw_input: String) -> Vec<i32> {
    raw_input
        .trim()
        .chars()
        .map(|c| (c as i32) - 0x30)
        .collect()
}

fn part_1(input: &[i32]) -> i32 {
    captcha(input, 1)
}

fn part_2(input: &[i32]) -> i32 {
    captcha(input, input.len() / 2)
}

fn captcha(input: &[i32], by: usize) -> i32 {
    let mut rotated = input.to_owned();
    rotated.rotate_left(by);
    input
        .iter()
        .zip(rotated.iter())
        .filter_map(|(a, b)| (a == b).then_some(*a))
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::io_helpers::read_input_from_resources;

    #[test]
    fn test_result_17_01() {
        let (year, day) = (2017, 1);
        let expected_result = (1089, 1156);

        let raw_input = read_input_from_resources(year, day, false);
        assert_eq!(solve_17_01(raw_input), expected_result);
    }
}
