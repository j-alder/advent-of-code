use std::fs;

pub fn format_soln_string(part1: String, part2: String) -> String {
    format!("\nPart 1: {}\nPart 2: {}\n", part1, part2)
}

pub fn read_input(year: &String, day: &String) -> String {
    let day_fmt = if day.len() == 1 {
        format!("0{}", day)
    } else {
        day.to_string()
    };
    return fs::read_to_string(format!("../input/y{}/d{}.txt", year, day_fmt)).unwrap();
}
