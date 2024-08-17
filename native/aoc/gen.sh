#!/bin/zsh

FULL_YEAR=$1
YEAR=${1:(-2)}
DAY=$(printf '%02d' $2)
SRC_FILE=./src/year_${FULL_YEAR}/day_${YEAR}_${DAY}.rs

if [[ ! -e $SRC_FILE ]]; then
    touch "$SRC_FILE"
    cat << EOF > "$SRC_FILE"
pub fn solve_${YEAR}_${DAY}(raw_input: String) -> (i32, i32) {
    let input = process(&raw_input);
    (part_1(&input), part_2(&input))
}

fn process(raw_input: &str) -> Vec<&str> {
    raw_input
        .trim()
        .split('\n')
        .collect()
}

fn part_1(input: &[&str]) -> i32 {
    input.len() as i32
}

fn part_2(input: &[&str]) -> i32 {
    input.len() as i32
}

#[cfg(test)]
mod tests {
    use crate::util::io_helpers::read_input_from_resources;
    use super::*;

    const YEAR: i16 = $1;
    const DAY: i8 = $2;

    #[test]
    fn test_process() {
        let given = "hello\nthere";
        let expected = vec!["hello", "there"];

        assert_eq!(process(given), expected);
    }

    #[test]
    fn test_solution() {
        let given = read_input_from_resources(YEAR, DAY, false);
        let expected = (3, 3);

        assert_eq!(solve_${YEAR}_${DAY}(given), expected);
    }
}
EOF
else
    echo "Source ${SRC_FILE} already exists"
fi
