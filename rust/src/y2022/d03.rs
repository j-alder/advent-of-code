use crate::util::format_soln_string;

fn get_priority(c: char) -> i32 {
    if c.is_uppercase() {
        c as i32 - 38
    } else {
        c as i32 - 96
    }
}

fn find_match_priority(c: (&str, &str)) -> i32 {
    for item in c.0.chars() {
        if c.1.contains(item) {
            return get_priority(item);
        }
    }
    return 0;
}

fn find_group_priority(c: (&String, &String, &String)) -> i32 {
    for item in c.0.chars() {
        if c.1.contains(item) && c.2.contains(item) {
            return get_priority(item);
        }
    }
    return 0;
}

pub fn soln(input: String) -> String {
    let part_one_sum = input.split('\n').fold(0, |a: i32, r| {
        let comps = r.split_at(r.len() / 2);
        a + find_match_priority(comps)
    });

    let rucksacks: Vec<String> = input.split('\n').map(|st| String::from(st)).collect();
    let mut part_two_sum = 0;

    // todo: use chunks?
    for x in (0..rucksacks.len() - 1).step_by(3) {
        let sacks = (&rucksacks[x], &rucksacks[x + 1], &rucksacks[x + 2]);
        part_two_sum += find_group_priority(sacks);
    }

    return format_soln_string(part_one_sum.to_string(), part_two_sum.to_string());
}
