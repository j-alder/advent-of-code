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
