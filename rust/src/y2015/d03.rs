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
