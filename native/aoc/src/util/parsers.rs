pub fn read_line_ints(raw_input: &str) -> Vec<i32> {
    raw_input
        .split('\n')
        .map(|x| x.parse::<i32>().unwrap())
        .collect()
}
