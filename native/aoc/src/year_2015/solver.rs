use crate::util::result::Solution;

use super::day_15_01::solve_15_01;
use super::day_15_02::solve_15_02;
use super::day_15_03::solve_15_03;

const YEAR: i16 = 2015;

pub fn run(day: i8, input_data: String) -> Solution {
    match day {
        1 => Solution::BothInt32(solve_15_01(input_data)),
        2 => Solution::BothInt32(solve_15_02(input_data)),
        3 => Solution::BothUSize(solve_15_03(input_data)),
        _ => panic!("Year {YEAR} day {day} has not been solved."),
    }
}
