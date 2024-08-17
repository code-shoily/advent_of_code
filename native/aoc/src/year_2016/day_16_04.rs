use itertools::Itertools;
use std::collections::HashMap;

pub fn solve_16_04(raw_input: String) -> (usize, usize) {
    let input = process(&raw_input);
    (part_1(&input), part_2(&input))
}

fn process(raw_input: &str) -> Vec<Room> {
    raw_input.split('\n').map(Room::new).collect()
}

fn part_1(input: &[Room]) -> usize {
    input
        .iter()
        .filter(|room| room.is_valid())
        .map(|room| room.sector_id)
        .sum()
}

fn part_2(input: &[Room]) -> usize {
    input
        .iter()
        .find_or_first(|room| room.has_north_pole_objects())
        .unwrap()
        .sector_id
}

#[derive(Debug, Eq, PartialEq)]
struct Room {
    encrypted_name: String,
    sector_id: usize,
    checksum: String,
}

impl Room {
    fn new(line: &str) -> Self {
        let tokens = line.split('-').collect::<Vec<&str>>();
        let suffix = tokens[tokens.len() - 1].split('[').collect::<Vec<&str>>();

        Room {
            encrypted_name: tokens[0..tokens.len() - 1].join(""),
            sector_id: suffix[0].parse::<usize>().unwrap(),
            checksum: suffix[1].replace(']', ""),
        }
    }

    fn is_valid(&self) -> bool {
        self.compute_checksum() == self.checksum
    }

    fn compute_checksum(&self) -> String {
        let frequencies = count_letters(&self.encrypted_name);
        let mut hash_vec: Vec<(&char, &usize)> = frequencies.iter().collect();

        hash_vec.sort_by(|a, b| {
            if a.1 == b.1 {
                a.0.cmp(b.0)
            } else {
                b.1.cmp(a.1)
            }
        });

        hash_vec.into_iter().map(|(a, _)| a).take(5).join("")
    }

    fn decrypted_name(&self) -> String {
        self.encrypted_name
            .chars()
            .map(|a| rotate(a, self.sector_id))
            .join("")
    }

    fn has_north_pole_objects(&self) -> bool {
        self.decrypted_name().contains("northpoleobjects")
    }
}

fn rotate(a: char, by: usize) -> char {
    let (a_value, z_value) = (b'a', b'z');
    let modulus = a as u8 + (by % 26) as u8;
    if modulus > z_value {
        (a_value - 1 + modulus % z_value) as char
    } else {
        modulus as char
    }
}

fn count_letters(word: &str) -> HashMap<char, usize> {
    let mut freqs: HashMap<char, usize> = HashMap::new();
    for ch in word.chars() {
        *freqs.entry(ch).or_insert(0) += 1;
    }

    freqs
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::io_helpers::read_input_from_resources;

    const YEAR: i16 = 2016;
    const DAY: i8 = 4;

    #[test]
    fn test_rotate() {
        let x = "qzmtzixmtkozyivhz".chars().map(|c| rotate(c, 343)).join("");
        println!("{}", x);
    }

    #[test]
    fn test_process() {
        let given = "aaaaa-bbb-z-y-x-123[abxyz]\na-b-c-d-e-f-g-h-987[abcde]";
        let room_1 = Room {
            encrypted_name: "aaaaabbbzyx".into(),
            sector_id: 123,
            checksum: "abxyz".into(),
        };
        let room_2 = Room {
            encrypted_name: "abcdefgh".into(),
            sector_id: 987,
            checksum: "abcde".into(),
        };

        let expected = vec![room_1, room_2];

        assert_eq!(process(given), expected);
    }

    #[test]
    fn test_compute_checksum() {
        let room = Room {
            encrypted_name: "aaaaabbbzyx".into(),
            sector_id: 123,
            checksum: "abxyz".into(),
        };
        assert_eq!(room.compute_checksum(), "abxyz");
    }

    #[test]
    fn test_room() {
        let given = "aaaaa-bbb-z-y-x-123[abxyz]";
        let expected = Room {
            encrypted_name: "aaaaabbbzyx".into(),
            sector_id: 123,
            checksum: "abxyz".into(),
        };

        assert_eq!(Room::new(given), expected);
    }

    #[test]
    fn test_solution() {
        let given = read_input_from_resources(YEAR, DAY, false);
        let expected = (158835, 993);

        assert_eq!(solve_16_04(given), expected);
    }
}
