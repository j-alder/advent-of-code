use crate::util::format_soln_string;
use std::collections::HashSet;

fn mv_santa(curr_loc: (i32, i32), dir: char) -> (i32, i32) {
    match dir {
        '^' => (curr_loc.0, curr_loc.1 + 1),
        '>' => (curr_loc.0 + 1, curr_loc.1),
        'v' => (curr_loc.0, curr_loc.1 - 1),
        '<' => (curr_loc.0 - 1, curr_loc.1),
        _ => curr_loc,
    }
}

pub fn soln(input: String) -> String {
    let mut coords = HashSet::from([(0, 0)]);

    let mut curr_loc = (0, 0);

    for d in input.chars() {
        curr_loc = mv_santa(curr_loc, d);
        coords.insert(curr_loc);
    }

    let total_one = coords.len();

    coords = HashSet::from([(0, 0)]);

    curr_loc = (0, 0);
    let mut curr_loc_bot = (0, 0);

    for (i, d) in input.chars().enumerate() {
        if i % 2 == 0 {
            curr_loc = mv_santa(curr_loc, d);
        } else {
            curr_loc_bot = mv_santa(curr_loc_bot, d);
        }
        coords.insert(curr_loc);
        coords.insert(curr_loc_bot);
    }

    let total_two = coords.len();

    return format_soln_string(total_one.to_string(), total_two.to_string());
}

/*
--- Day 3: Perfectly Spherical Houses in a Vacuum ---
Santa is delivering presents to an infinite two-dimensional grid of houses.

He begins by delivering a present to the house at his starting location, and then
an elf at the North Pole calls him via radio and tells him where to move next. Moves
are always exactly one house to the north (^), south (v), east (>), or west (<).
After each move, he delivers another present to the house at his new location.

However, the elf back at the north pole has had a little too much eggnog, and so his
directions are a little off, and Santa ends up visiting some houses more than once.
How many houses receive at least one present?

For example:

> delivers presents to 2 houses: one at the starting location, and one to the east.
^>v< delivers presents to 4 houses in a square, including twice to the house at his starting/ending location.
^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2 houses.

--- Part Two ---
The next year, to speed up the process, Santa creates a robot version of himself,
Robo-Santa, to deliver presents with him.

Santa and Robo-Santa start at the same location (delivering two presents to the
same starting house), then take turns moving based on instructions from the elf,
who is eggnoggedly reading from the same script as the previous year.

This year, how many houses receive at least one present?

For example:

^v delivers presents to 3 houses, because Santa goes north, and then Robo-Santa goes south.
^>v< now delivers presents to 3 houses, and Santa and Robo-Santa end up back where they started.
^v^v^v^v^v now delivers presents to 11 houses, with Santa going one direction and Robo-Santa going the other.
*/
