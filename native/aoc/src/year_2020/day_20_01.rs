use itertools::Itertools;

pub fn solve_20_01(raw_input: String) -> (i32, i32) {
    let processed_input = process(raw_input);

    (part_1(&processed_input), part_2(&processed_input))
}

fn process(raw_input: String) -> Vec<i32> {
    raw_input
        .split('\n')
        .map(|i| i.parse::<i32>().unwrap())
        .sorted()
        .collect()
}

fn part_1(input: &[i32]) -> i32 {
    let (value_1, value_2) = get_pairs_summing_to(input, 2020).unwrap();
    value_1 * value_2
}

fn part_2(input: &[i32]) -> i32 {
    for (idx, val_0) in input.iter().enumerate() {
        let remaining = 2020 - val_0;
        match get_pairs_summing_to(&input[idx..], remaining) {
            Some((val_1, val_2)) => return val_0 * val_1 * val_2,
            None => continue,
        }
    }
    unreachable!()
}

fn get_pairs_summing_to(numbers: &[i32], target: i32) -> Option<(i32, i32)> {
    let (mut l_idx, mut r_idx) = (0_usize, numbers.len() - 1);

    while l_idx < r_idx {
        match numbers[l_idx] + numbers[r_idx] {
            i if i < target => l_idx += 1,
            i if i > target => r_idx -= 1,
            _ => return Some((numbers[l_idx], numbers[r_idx])),
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::io_helpers::read_input_from_resources;

    #[test]
    fn test_result_20_01() {
        let (year, day) = (2020, 1);
        let expected_result = (1014624, 80072256);

        let raw_input = read_input_from_resources(year, day, false);
        assert_eq!(solve_20_01(raw_input), expected_result);
    }
}
