use crate::util::result::Solution;

use super::day_20_01::solve_20_01;
use super::day_20_02::solve_20_02;

const YEAR: i16 = 2020;

pub fn run(day: i8, input_data: String) -> Solution {
    match day {
        1 => Solution::BothInt32(solve_20_01(input_data)),
        2 => Solution::BothUSize(solve_20_02(input_data)),
        _ => panic!("Year {YEAR} day {day} has not been solved."),
    }
}
