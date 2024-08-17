use std::fs;

pub const MAX_YEAR: i16 = 2023;

pub fn read_input_from_resources(year: i16, day: i8, from_elixir: bool) -> String {
    let input_path = if from_elixir {
        format!("./priv/input_files/{year}_{day}.txt")
    } else {
        format!("../../priv/input_files/{year}_{day}.txt")
    };
    read_input_from_path(input_path)
}

pub fn read_input_from_path(input_path: String) -> String {
    fs::read_to_string(input_path).expect("File doesn't exist.")
}

pub fn validate_input(year: i16, day: i8) -> (bool, bool) {
    let mut year_ok = true;
    let mut day_ok = true;

    if !(2015..=MAX_YEAR).contains(&year) {
        year_ok = false;
    }

    if !(1..=25).contains(&day) {
        day_ok = false;
    }

    (year_ok, day_ok)
}
