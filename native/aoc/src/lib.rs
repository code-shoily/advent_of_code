pub mod util {
    pub mod io_helpers;
    pub mod parsers;
    pub mod runner;
    pub mod result;
}

// All modules for year 2015
pub mod year_2015 {
    mod day_15_01;
    mod day_15_02;
    mod day_15_03;
    pub mod solver;
}

// All modules for year 2016
pub mod year_2016 {
    mod day_16_02;
    mod day_16_04;
    pub mod solver;
}

// All modules for year 2017
pub mod year_2017 {
    mod day_17_04;
    pub mod solver;
}

// All modules for year 2018
pub mod year_2018 {
    mod day_18_01;
    pub mod solver;
}

// All modules for year 2019
pub mod year_2019 {
    mod day_19_01;
    pub mod solver;
}

// All modules for year 2020
pub mod year_2020 {
    mod day_20_01;
    mod day_20_02;
    pub mod solver;
}

// All modules for year 2021
pub mod year_2021 {
    mod day_21_01;
    mod day_21_02;
    pub mod solver;
}

// All modules for year 2022
pub mod year_2022 {
    mod day_22_01;
    pub mod solver;
}

// All modules for year 2023
pub mod year_2023 {
    mod day_23_01;
    pub mod solver;
}

use util::io_helpers::read_input_from_resources;
use util::result::Solution;
use util::runner::Runner;
use rustler::Error;

#[rustler::nif]
fn solve(year: i16, day: i8) -> Result<Solution, Error> {
    let content = read_input_from_resources(year, day, true);
    Result::Ok(Runner::new(year, day, content).run())
}

rustler::init!("Elixir.AdventOfCode.RustNif");
