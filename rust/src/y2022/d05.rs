use crate::util::format_soln_string;
use std::cmp::max;

const CHAR_WIDTH: usize = 4;

/**
 * Parse a single line representing a strata of the initial
 * cargo stack state.
 */
fn parse_initial_line(ln: &str, vecs: &mut Vec<Vec<char>>) {
    let mut i = 0; // todo: potentially unnecessary memory use
    for n in (0..ln.len()).step_by(CHAR_WIDTH) {
        let idx = n + CHAR_WIDTH - 3;
        let c = ln.chars().nth(idx).unwrap();
        if c != ' ' {
            vecs[i].insert(0, c);
        }
        i += 1;
    }
}

/**
 * Reads a string representation of the initial state into a
 * Vec containing char Vecs representing cargo stacks
 */
fn parse_initial_state(raw: Option<&str>) -> Vec<Vec<char>> {
    let state_raw = raw.unwrap().split('\n');
    let line_width = state_raw.clone().nth(0).unwrap().len();
    let state_height = state_raw.collect::<Vec<&str>>().len();
    let vecs = (line_width + 1) / CHAR_WIDTH;
    let mut res: Vec<Vec<char>> = Vec::new();
    for _ in 0..vecs {
        res.push(Vec::new());
    }
    raw.unwrap()
        .split('\n')
        .take(state_height - 1)
        .for_each(|ln| {
            parse_initial_line(ln, &mut res);
        });
    return res;
}

/**
 * Parse an instruction, e.g. "move 6 from 6 to 5" into an
 * operable triple representing:
 *
 * 1. how many to move
 * 2. from which stack
 * 3. to which stack
 *
 * The to/from stacks are decremented by one to conform to
 * zero-indexed Vecs.
 */
fn parse_inst(inst_ln: &str) -> (usize, usize, usize) {
    let x: Vec<&str> = inst_ln.split(' ').collect();
    return (
        x[1].parse::<usize>().unwrap(),
        x[3].parse::<usize>().unwrap() - 1,
        x[5].parse::<usize>().unwrap() - 1,
    );
}

fn part_one(raw_inst: &Vec<&str>, mut stacks: Vec<Vec<char>>) -> String {
    for ri in raw_inst {
        let inst = parse_inst(ri);
        for _ in 0..inst.0 {
            let x = stacks[inst.1].pop();
            if x == None {
                break;
            }
            stacks[inst.2].push(x.unwrap());
        }
    }
    return String::from_iter(stacks.iter().map(|v| v.last().unwrap()));
}

fn part_two(raw_inst: &Vec<&str>, mut stacks: Vec<Vec<char>>) -> String {
    for ri in raw_inst {
        let inst = parse_inst(ri);
        if stacks[inst.1].len() > 0 {
            let range = max(0, stacks[inst.1].len() - inst.0)..stacks[inst.1].len()-1;
            let mut substack: Vec<char> = stacks[inst.1].splice(range, []).collect();
            stacks[inst.2].append(&mut substack);
        }
    }
    return String::from_iter(stacks.iter().map(|v| v.last().unwrap()));
}

pub fn soln(input: String) -> String {
    let mut raw = input.split("\n\n");
    let stacks1 = parse_initial_state(raw.clone().nth(0));
    let stacks2 = parse_initial_state(raw.clone().nth(0));
    let raw_inst: Vec<&str> = raw.nth(1).unwrap().split('\n').filter(|s| !s.is_empty()).collect();

    return format_soln_string(
        part_one(&raw_inst, stacks1), 
        part_two(&raw_inst, stacks2)
    );
}
