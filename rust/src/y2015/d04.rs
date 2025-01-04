use md5::Digest;

use crate::util::format_soln_string;

fn get_key(n: u32, base: &str) -> String {
    format!("{}{}", base, n.to_string())
}

fn digest_leading_zeroes(d: Digest, n: usize) -> bool {
    // todo: could be refactored to not use format and instead compare bytes
    format!("{:x}", d).starts_with("0".repeat(n).as_str())
}

fn find_soln(leading_zeroes: usize, input: &String) -> u32 {
    let mut n = 1;
    let mut key = get_key(n, &input);
    let mut digest = md5::compute(key.as_bytes());
    while !digest_leading_zeroes(digest, leading_zeroes) {
        n += 1;
        key = get_key(n, &input);
        digest = md5::compute(key.as_bytes());
    }
    return n;
}

pub fn soln(input: String) -> String {
    let soln1 = find_soln(5, &input);
    let soln2 = find_soln(6, &input);
    return format_soln_string(
        String::from(soln1.to_string()),
        String::from(soln2.to_string()),
    );
}
