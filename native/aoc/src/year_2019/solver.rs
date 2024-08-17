use crate::util::result::Solution;

use super::day_19_01::solve_19_01;

const YEAR: i16 = 2019;

pub fn run(day: i8, input_data: String) -> Solution {
    match day {
        1 => Solution::BothInt32(solve_19_01(input_data)),
        _ => panic!("Year {YEAR} day {day} has not been solved."),
    }
}
