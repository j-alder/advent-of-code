pub mod d01;
pub mod d02;
pub mod d03;
pub mod d04;
pub mod d05;
pub mod d13;

use d01::soln as soln1;
use d02::soln as soln2;
use d03::soln as soln3;
use d04::soln as soln4;
use d05::soln as soln5;
use d13::soln as soln13;

pub fn get_solution_for_day(day: &str, input: String) -> String {
    match day {
        "1" => soln1(input),
        "2" => soln2(input),
        "3" => soln3(input),
        "4" => soln4(input),
        "5" => soln5(input),
        "13" => soln13(input),
        _ => String::from("No solution found"),
    }
}
