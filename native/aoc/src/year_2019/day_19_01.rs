use crate::util::parsers::read_line_ints;

pub fn solve_19_01(raw_input: String) -> (i32, i32) {
    let input = process(raw_input);
    (part_1(&input), part_2(&input))
}

fn process(raw_input: String) -> Vec<i32> {
    read_line_ints(&raw_input)
}

fn part_1(input: &[i32]) -> i32 {
    input.iter().map(|mass| get_fuel_1(*mass)).sum()
}

fn part_2(input: &[i32]) -> i32 {
    input.iter().map(|mass| get_fuel_2(*mass)).sum()
}

fn get_fuel_1(mass: i32) -> i32 {
    match (mass / 3) - 2 {
        i if i <= 0 => 0,
        fuel => fuel,
    }
}

fn get_fuel_2(mass: i32) -> i32 {
    let mut total_fuel = 0;
    let mut computed_mass = mass;

    loop {
        let fuel = get_fuel_1(computed_mass);
        total_fuel += fuel;
        if fuel == 0 {
            return total_fuel;
        }
        computed_mass = fuel;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::io_helpers::read_input_from_resources;

    #[test]
    fn test_result_19_01() {
        let (year, day) = (2019, 1);
        let expected_result = (3421505, 5129386);

        let raw_input = read_input_from_resources(year, day, false);
        assert_eq!(solve_19_01(raw_input), expected_result);
    }
}
