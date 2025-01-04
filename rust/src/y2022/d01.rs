use crate::util::format_soln_string;

pub fn soln(input: String) -> String {
    let mut tmp: u32 = 0;
    let mut top_three: [u32; 3] = [0, 0, 0];

    for c in input.split('\n').into_iter() {
        if c == "" {
            if tmp >= top_three[0] {
                top_three[2] = top_three[1];
                top_three[1] = top_three[0];
                top_three[0] = tmp;
            } else if tmp > top_three[1] {
                top_three[2] = top_three[1];
                top_three[1] = tmp;
            } else if tmp > top_three[2] {
                top_three[2] = tmp;
            }
            tmp = 0;
        } else {
            let v: u32 = c.parse().unwrap();
            tmp += v;
        }
    }

    let s: u32 = top_three.iter().sum();

    return format_soln_string(top_three[0].to_string(), s.to_string());
}
