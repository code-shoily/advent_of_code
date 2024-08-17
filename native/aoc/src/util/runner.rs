use crate::{
    year_2015, year_2016, year_2017, year_2018, year_2019, year_2020, year_2021, year_2022,
    year_2023,
};

use crate::util::io_helpers;
use crate::util::result::Solution;

pub struct Runner {
    year: i16,
    day: i8,
    input_data: String,
}

impl Runner {
    pub fn new(year: i16, day: i8, input_data: String) -> Runner {
        Runner {
            year,
            day,
            input_data,
        }
    }

    fn solve(&self) -> Solution {
        let day = self.day;
        let input_data = String::from(&self.input_data);

        match self.year {
            2015 => year_2015::solver::run(day, input_data),
            2016 => year_2016::solver::run(day, input_data),
            2017 => year_2017::solver::run(day, input_data),
            2018 => year_2018::solver::run(day, input_data),
            2019 => year_2019::solver::run(day, input_data),
            2020 => year_2020::solver::run(day, input_data),
            2021 => year_2021::solver::run(day, input_data),
            2022 => year_2022::solver::run(day, input_data),
            2023 => year_2023::solver::run(day, input_data),
            _ => panic!("Invalid year"),
        }
    }

    pub fn run(&self) -> Solution {
        match io_helpers::validate_input(self.year, self.day) {
            (true, true) => self.solve(),
            (true, false) => panic!("Invalid input for day => {}", self.day),
            (false, true) => panic!("Invalid input for year => {}", self.year),
            (false, false) => panic!("Invalid input (year/day) => {}/{}", self.year, self.day),
        }
    }
}
