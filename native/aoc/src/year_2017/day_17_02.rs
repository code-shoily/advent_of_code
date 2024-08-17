use itertools::Itertools;

pub fn solve_17_02(raw_input: String) -> (i32, i32) {
    let input = process(&raw_input);
    (part_1(&input), part_2(&input))
}

fn process(input_data: &str) -> Vec<Vec<i32>> {
    input_data.trim().split('\n').map(parse_row).collect()
}

fn parse_row(line: &str) -> Vec<i32> {
    line.split('\t')
        .map(|cell| cell.parse::<i32>().unwrap())
        .sorted()
        .collect()
}

fn part_1(input_data: &[Vec<i32>]) -> i32 {
    input_data
        .iter()
        .map(|row| row.last().unwrap() - row.first().unwrap())
        .sum()
}

fn part_2(input_data: &[Vec<i32>]) -> i32 {
    input_data
        .iter()
        .map(|values| {
            for i in 0..values.len() {
                for j in i + 1..values.len() {
                    if values[j] % values[i] == 0 {
                        return values[j] / values[i];
                    }
                }
            }
            unreachable!()
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::io_helpers::read_input_from_resources;

    #[test]
    fn test_result_17_02() {
        let raw_input = read_input_from_resources(2017, 2, false);
        let expected_result = (32020, 236);

        assert_eq!(solve_17_02(raw_input), expected_result);
    }
}
