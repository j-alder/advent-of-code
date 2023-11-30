use crate::util::format_soln_string;

enum Operation {
    Toggle,
    TurnOn,
    TurnOff,
}

type Coord = (usize, usize);

type Grid = [[bool; 1000]; 1000];

fn mutate(mut g: Grid, op: Operation, s: Coord, e: Coord) -> Grid {
    let mut x = s.0;
    let mut y = s.1;
    while x <= e.0 {
        while y <= e.1 {
            g[x][y] = match op {
                Operation::Toggle => !g[x][y],
                Operation::TurnOn => true,
                Operation::TurnOff => false,
            };
            y += 1;
        }
        x += 1;
    }
    return g;
}

fn count_lit(g: Grid) -> u32 {
    let mut count = 0;
    for row in &g {
        for light in row {
            count += if *light == true { 1 } else { 0 }
        }
    }
    return count;
}

fn get_coords(coord_str: &str) -> Coord {
    let coords = coord_str
        .split(",")
        .map(|x| str::parse::<usize>(x).unwrap())
        .collect::<Vec<usize>>();
    return (coords[0], coords[1]);
}

pub fn soln(input: String) -> String {
    let mut grid: Grid = [[false; 1000]; 1000];

    let lines = input.split("\n");

    for line in lines {
        let inst: Vec<&str> = line.split(" ").collect();
        match inst[0] {
            "toggle" => {
                let start_pos = get_coords(inst[1]);
                let end_pos = get_coords(inst[3]);
                grid = mutate(grid, Operation::Toggle, start_pos, end_pos);
            }
            _ => {
                let start_pos = get_coords(inst[2]);
                let end_pos = get_coords(inst[4]);
                match inst[1] {
                    "on" => {
                        grid = mutate(grid, Operation::TurnOn, start_pos, end_pos);
                    }
                    _ => {
                        grid = mutate(grid, Operation::TurnOff, start_pos, end_pos);
                    }
                }
            }
        }
    }

    let count = count_lit(grid).to_string();

    return format_soln_string(count, String::from("Incomplete"));
}

/*
--- Day 6: Probably a Fire Hazard ---

Because your neighbors keep defeating you in the holiday house decorating contest
year after year, you've decided to deploy one million lights in a 1000x1000 grid.

Furthermore, because you've been especially nice this year, Santa has mailed you
instructions on how to display the ideal lighting configuration.

Lights in your grid are numbered from 0 to 999 in each direction; the lights at
each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include whether
to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs.
Each coordinate pair represents opposite corners of a rectangle, inclusive; a
coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square.
The lights all start turned off.

To defeat your neighbors this year, all you have to do is set up your lights by
doing the instructions Santa sent you in order.

For example:

    turn on 0,0 through 999,999 would turn on (or leave on) every light.
    toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off.
    turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.

After following the instructions, how many lights are lit?

*/
