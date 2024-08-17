use crate::util::result::Solution;

use super::day_16_02::solve_16_02;
use super::day_16_04::solve_16_04;

const YEAR: i16 = 2016;

pub fn run(day: i8, input_data: String) -> Solution {
    match day {
        2 => Solution::BothString(solve_16_02(input_data)),
        4 => Solution::BothUSize(solve_16_04(input_data)),
        _ => panic!("Year {YEAR} day {day} has not been solved."),
    }
}
