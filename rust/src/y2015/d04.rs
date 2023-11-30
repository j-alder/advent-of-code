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

/*
--- Day 4: The Ideal Stocking Stuffer ---
Santa needs help mining some AdventCoins (very similar to bitcoins) to use as
gifts for all the economically forward-thinking little girls and boys.

To do this, he needs to find MD5 hashes which, in hexadecimal, start with at
least five zeroes. The input to the MD5 hash is some secret key (your puzzle
input, given below) followed by a number in decimal. To mine AdventCoins, you
must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...)
that produces such a hash.

For example:

If your secret key is abcdef, the answer is 609043, because the MD5 hash of
abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest
such number to do so.

If your secret key is pqrstuv, the lowest number it combines with to make an
MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of
pqrstuv1048970 looks like 000006136ef....

Your puzzle input is yzbqklnj.

--- Part Two ---
Now find one that starts with six zeroes.
*/
