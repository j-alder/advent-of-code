mod util;
mod y2015;
mod y2022;

use crate::util::read_input;

use std::env;
use y2015::get_solution_for_day as get_solution_for_day_2015;
use y2022::get_solution_for_day as get_solution_for_day_2022;

fn get_solution_for_year_and_day(year: &str, day: &str, input: String) -> String {
    match year {
        "2015" => get_solution_for_day_2015(day, input),
        "2022" => get_solution_for_day_2022(day, input),
        _ => String::from("No solution found"),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let year = &args[1];
    let day = &args[2];

    let input = read_input(year, day);

    let soln = get_solution_for_year_and_day(year, day, input);
    println!("{soln}");
}
