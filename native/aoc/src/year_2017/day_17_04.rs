use itertools::Itertools;
use std::collections::HashSet;
use std::str::Split;

pub fn solve_17_04(raw_input: String) -> (usize, usize) {
    let input = process(&raw_input);
    (part_1(&input), part_2(&input))
}

fn process(raw_input: &str) -> Vec<&str> {
    raw_input.split('\n').collect()
}

fn part_1(input: &[&str]) -> usize {
    input
        .iter()
        .filter(|&line| has_no_duplicate_words(line.split(' ')))
        .count()
}

fn part_2(input: &[&str]) -> usize {
    input
        .iter()
        .filter(|&line| has_no_duplicate_anagrams(line.split(' ')))
        .count()
}

fn has_no_duplicate_words(words: Split<char>) -> bool {
    let mut history: HashSet<&str> = HashSet::new();
    for word in words {
        if !history.insert(word) {
            return false;
        }
    }
    true
}

fn has_no_duplicate_anagrams(words: Split<char>) -> bool {
    has_no_duplicate_words(
        words
            .map(|word| word.chars().sorted().join(""))
            .join(" ")
            .split(' '),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::io_helpers::read_input_from_resources;

    const YEAR: i16 = 2017;
    const DAY: i8 = 4;

    #[test]
    fn test_process() {
        let given = "hello\nthere";
        let expected = vec!["hello", "there"];

        assert_eq!(process(given), expected);
    }

    #[test]
    fn test_solution() {
        let given = read_input_from_resources(YEAR, DAY, false);
        let expected = (456, 187);

        assert_eq!(solve_17_04(given), expected);
    }
}
