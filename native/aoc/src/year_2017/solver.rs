use crate::util::result::Solution;

use super::day_17_04::solve_17_04;

const YEAR: i16 = 2017;

pub fn run(day: i8, input_data: String) -> Solution {
    match day {
        4 => Solution::BothUSize(solve_17_04(input_data)),
        _ => panic!("Year {YEAR} day {day} has not been solved."),
    }
}
