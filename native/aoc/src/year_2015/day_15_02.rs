use std::cmp::min;

pub fn solve_15_02(raw_input: String) -> (i32, i32) {
    let input = process(&raw_input);
    (part_1(&input), part_2(&input))
}

#[derive(Debug, Eq, PartialEq)]
struct Dims {
    l: i32,
    w: i32,
    h: i32,
}

impl Dims {
    fn new(l: i32, w: i32, h: i32) -> Self {
        Dims { l, w, h }
    }

    fn wrapping_paper(&self) -> i32 {
        self.area_of_smallest_side() + self.surface_area()
    }

    fn ribbon(&self) -> i32 {
        self.perimeter_of_smallest_sides() + self.volume()
    }

    fn surface_area(&self) -> i32 {
        let (l, h, w) = self.params();
        2 * (l * h + h * w + w * l)
    }

    fn volume(&self) -> i32 {
        let (l, h, w) = self.params();
        l * h * w
    }

    fn perimeter_of_smallest_sides(&self) -> i32 {
        let (smallest_1, smallest_2) = self.smallest_sides();

        2 * (smallest_1 + smallest_2)
    }

    fn area_of_smallest_side(&self) -> i32 {
        let (smallest_1, smallest_2) = self.smallest_sides();

        smallest_1 * smallest_2
    }

    fn params(&self) -> (i32, i32, i32) {
        (self.l, self.w, self.h)
    }

    fn smallest_sides(&self) -> (i32, i32) {
        let (l, h, w) = self.params();
        let smallest_1 = min(l, min(l, w));
        let smallest_2 = match smallest_1 {
            i if i == l => min(h, w),
            i if i == h => min(l, w),
            i if i == w => min(l, h),
            _ => unreachable!(),
        };
        (smallest_1, smallest_2)
    }
}

fn process(input_data: &str) -> Vec<Dims> {
    input_data.split('\n').map(parse_dims).collect()
}

fn parse_dims(line: &str) -> Dims {
    let dims: Vec<i32> = line
        .split('x')
        .map(|dim| dim.parse::<i32>().unwrap())
        .collect();

    Dims::new(dims[0], dims[1], dims[2])
}

fn part_1(input_data: &[Dims]) -> i32 {
    input_data.iter().map(|d| d.wrapping_paper()).sum()
}

fn part_2(input_data: &[Dims]) -> i32 {
    input_data.iter().map(|d| d.ribbon()).sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::io_helpers::read_input_from_resources;

    #[test]
    fn test_surface_area() {
        assert_eq!(Dims::new(2, 3, 4).surface_area(), 52);
        assert_eq!(Dims::new(1, 1, 10).surface_area(), 42);
    }

    #[test]
    fn test_area_of_smallest_side() {
        assert_eq!(Dims::new(2, 3, 4).area_of_smallest_side(), 6);
        assert_eq!(Dims::new(1, 1, 10).area_of_smallest_side(), 1);
    }

    #[test]
    fn test_dims_is_parsed() {
        assert_eq!(parse_dims("10x34x2"), Dims::new(10, 34, 2));
    }

    #[test]
    fn raw_inputs_are_parsed() {
        let raw_input = "1x2x3\n40x1x1\n1x40x1\n1x1x40";
        let expected: Vec<Dims> = vec![
            Dims::new(1, 2, 3),
            Dims::new(40, 1, 1),
            Dims::new(1, 40, 1),
            Dims::new(1, 1, 40),
        ];
        assert_eq!(process(raw_input), expected);
    }

    #[test]
    fn test_result_15_02() {
        let raw_input = read_input_from_resources(2015, 2, false);
        let expected_result = (1606483, 3842356);

        assert_eq!(solve_15_02(raw_input), expected_result);
    }
}
