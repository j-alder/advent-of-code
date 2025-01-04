use crate::util::format_soln_string;

pub fn soln(input: String) -> String {
    let mut f = 0;
    let mut b: usize = 0;

    for (i, c) in input.chars().enumerate() {
        f += match c {
            '(' => 1,
            ')' => -1,
            _ => 0,
        };
        if b == 0 && f == -1 {
            b = i + 1;
        }
    }

    return format_soln_string(f.to_string(), b.to_string());
}
