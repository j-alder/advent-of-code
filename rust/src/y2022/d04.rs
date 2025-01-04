use crate::util::format_soln_string;

fn parse_section(section_raw: &str) -> (i32, i32) {
    if section_raw.is_empty() {
        return (0, 0);
    }
    let sec_vec: Vec<i32> = section_raw
        .split('-')
        .map(|s| s.parse::<i32>().unwrap())
        .collect();
    return (sec_vec[0], sec_vec[1]);
}

fn parse_sections(sections_raw: &str) -> ((i32, i32), (i32, i32)) {
    let sections: Vec<&str> = sections_raw.split(',').collect();
    return (parse_section(sections[0]), parse_section(sections[1]));
}

fn fully_contains(a: (i32, i32), b: (i32, i32)) -> bool {
    (a.0..=a.1).contains(&b.0) && (a.0..=a.1).contains(&b.1)
        || (b.0..=b.1).contains(&a.0) && (b.0..=b.1).contains(&a.1)
}

fn partially_contains(a: (i32, i32), b: (i32, i32)) -> bool {
    (a.0..=a.1).contains(&b.0)
        || (a.0..=a.1).contains(&b.1)
        || (b.0..=b.1).contains(&a.0)
        || (b.0..=b.1).contains(&a.1)
}

pub fn soln(input: String) -> String {
    let pairs_raw = input.split('\n').filter(|splt| !splt.is_empty());

    let ans = pairs_raw.fold((0, 0), |a: (i32, i32), st| {
        let mut r = a;
        let s = parse_sections(st);
        if fully_contains(s.0, s.1) {
            r = (r.0 + 1, r.1 + 1);
        } else if partially_contains(s.0, s.1) {
            r = (r.0, r.1 + 1);
        }
        return r;
    });

    return format_soln_string(ans.0.to_string(), ans.1.to_string());
}
