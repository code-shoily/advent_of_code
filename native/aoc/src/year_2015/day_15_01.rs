pub fn solve_15_01(raw_input: String) -> (i32, i32) {
    let input = process(&raw_input);
    (part_1(&input), part_2(&input))
}

fn process(input_data: &str) -> Vec<i32> {
    input_data
        .chars()
        .map(|b| match b {
            '(' => 1,
            ')' => -1,
            _ => 0,
        })
        .collect()
}
fn part_1(input_data: &[i32]) -> i32 {
    input_data.iter().sum()
}

fn part_2(input_data: &[i32]) -> i32 {
    let mut floor = 0;

    for (i, x) in input_data.iter().enumerate() {
        floor += x;

        if floor < 0 {
            return (i as i32) + 1;
        }
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::io_helpers::read_input_from_resources;

    #[test]
    fn test_result_15_01() {
        let raw_input = read_input_from_resources(2015, 1, false);
        let expected_result = (232, 1783);

        assert_eq!(solve_15_01(raw_input), expected_result);
    }
}
