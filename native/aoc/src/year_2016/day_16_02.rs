pub fn solve_16_02(raw_input: String) -> (String, String) {
    let input = process(&raw_input);
    (part_1(&input), part_2(&input))
}

#[derive(Debug, Eq, PartialEq)]
enum MoveTo {
    Up,
    Down,
    Left,
    Right,
}

fn process(raw_input: &str) -> Vec<Vec<MoveTo>> {
    raw_input
        .split('\n')
        .map(|line| {
            line.chars()
                .map(|c| match c {
                    'U' => MoveTo::Up,
                    'D' => MoveTo::Down,
                    'L' => MoveTo::Left,
                    'R' => MoveTo::Right,
                    _ => panic!("GOT {}", c as i8),
                })
                .collect()
        })
        .collect()
}

const NULL_VALUE: &str = "0";
fn part_1(input: &[Vec<MoveTo>]) -> String {
    let grid = vec![
        vec!["0", "0", "0", "0", "0"],
        vec!["0", "1", "2", "3", "0"],
        vec!["0", "4", "5", "6", "0"],
        vec!["0", "7", "8", "9", "0"],
        vec!["0", "0", "0", "0", "0"],
    ];

    let (row, col) = (2, 2);

    decrypt_code(input, &grid, row, col)
}

fn part_2(input: &[Vec<MoveTo>]) -> String {
    let grid = vec![
        vec!["0", "0", "0", "0", "0", "0", "0"],
        vec!["0", "0", "0", "1", "0", "0", "0"],
        vec!["0", "0", "2", "3", "4", "0", "0"],
        vec!["0", "5", "6", "7", "8", "9", "0"],
        vec!["0", "0", "A", "B", "C", "0", "0"],
        vec!["0", "0", "0", "D", "0", "0", "0"],
        vec!["0", "0", "0", "0", "0", "0", "0"],
    ];

    let (row, col) = (3, 1);

    decrypt_code(input, &grid, row, col)
}

fn decrypt_code(
    input: &[Vec<MoveTo>],
    grid: &[Vec<&str>],
    mut row: usize,
    mut col: usize,
) -> String {
    let mut output: Vec<&str> = vec![];

    for line in input {
        for instruction in line {
            match instruction {
                MoveTo::Up if grid[row - 1][col] != NULL_VALUE => row -= 1,
                MoveTo::Down if grid[row + 1][col] != NULL_VALUE => row += 1,
                MoveTo::Left if grid[row][col - 1] != NULL_VALUE => col -= 1,
                MoveTo::Right if grid[row][col + 1] != NULL_VALUE => col += 1,
                _ => continue,
            }
        }
        output.push(grid[row][col])
    }

    output.join("")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::io_helpers::read_input_from_resources;

    #[test]
    fn test_process() {
        let given = "LLRL\nRR";
        let expected = vec![
            vec![MoveTo::Left, MoveTo::Left, MoveTo::Right, MoveTo::Left],
            vec![MoveTo::Right, MoveTo::Right],
        ];

        assert_eq!(process(given), expected);
    }

    #[test]
    fn test_solution() {
        let raw_input = read_input_from_resources(2016, 2, false);
        let output = (String::from("76792"), String::from("A7AC3"));
        assert_eq!(solve_16_02(raw_input), output);
    }
}
