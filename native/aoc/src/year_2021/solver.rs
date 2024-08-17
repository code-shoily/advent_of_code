use crate::util::result::Solution;

use super::day_21_01::solve_21_01;
use super::day_21_02::solve_21_02;

const YEAR: i16 = 2021;

pub fn run(day: i8, input_data: String) -> Solution {
    match day {
        1 => Solution::BothUSize(solve_21_01(input_data)),
        2 => Solution::BothInt32(solve_21_02(input_data)),
        _ => panic!("Year {YEAR} day {day} has not been solved."),
    }
}
