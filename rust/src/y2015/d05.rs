use crate::util::format_soln_string;
use std::collections::HashSet;

fn has_three_vowels(s: &str) -> bool {
    let mut vowels = 0;
    for c in s.chars() {
        match c {
            'a' | 'e' | 'i' | 'o' | 'u' => vowels += 1,
            _ => {}
        }
        if vowels >= 3 {
            break;
        }
    }
    return vowels >= 3;
}

fn has_repeating_letter(s: &str) -> bool {
    if s.len() <= 1 {
        return false;
    } else if s.chars().nth(0) == s.chars().nth(1) {
        return true;
    } else {
        return has_repeating_letter(&s[1..]);
    }
}

fn has_contraband(s: &str) -> bool {
    if s.len() <= 1 {
        false
    } else {
        match &s[0..2] {
            "ab" | "cd" | "pq" | "xy" => true,
            _ => has_contraband(&s[1..]),
        }
    }
}

fn has_repeating_pair(s: &str) -> bool {
    let mut x: HashSet<&str> = HashSet::new();
    let mut i = 1;
    while i < s.len() - 1 {
        if x.contains(&s[i..i + 2]) {
            return true;
        }
        x.insert(&s[i..i + 2]);
        i += 1;
    }
    return false;
}

fn has_palindrome(s: &str) -> bool {
    let mut i = 0;
    while i < s.len() - 3 {
        if s.chars().nth(i) == s.chars().nth(i + 2) {
            return true;
        }
        i += 1;
    }
    return false;
}

pub fn soln(input: String) -> String {
    let mut nice_one = 0;
    for s in input.split("\n") {
        if !has_contraband(s) && has_repeating_letter(s) && has_three_vowels(s) {
            nice_one += 1;
        }
    }
    let mut nice_two = 0;
    for s in input.split("\n") {
        if has_palindrome(s) && has_repeating_pair(s) {
            println!("{s}");
            nice_two += 1;
        }
    }
    return format_soln_string(nice_one.to_string(), nice_two.to_string());
}

/*
--- Day 5: Doesn't He Have Intern-Elves For This? ---
Santa needs help figuring out which strings in his text file are naughty or 
nice.

A nice string is one with all of the following properties:

It contains at least three vowels (aeiou only), like aei, xazegov, or 
aeiouaeiouaeiou.
It contains at least one letter that appears twice in a row, like xx, abcdde 
(dd), or aabbccdd (aa, bb, cc, or dd).
It does not contain the strings ab, cd, pq, or xy, even if they are part of one 
of the other requirements.

For example:

- ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), 
  a double letter (...dd...), and none of the disallowed substrings.
- aaa is nice because it has at least three vowels and a double letter, even 
  though the letters used by different rules overlap.
- jchzalrnumimnmhp is naughty because it has no double letter.
- haegwjzuvuyypxyu is naughty because it contains the string xy.
- dvszwmarrgswjxmb is naughty because it contains only one vowel.

How many strings are nice?

--- Part Two ---
Realizing the error of his ways, Santa has switched to a better model of 
determining whether a string is naughty or nice. None of the old rules apply, 
as they are all clearly ridiculous.

Now, a nice string is one with all of the following properties:

- It contains a pair of any two letters that appears at least twice in the 
  string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like 
  aaa (aa, but it overlaps).
- It contains at least one letter which repeats with exactly one letter between 
  them, like xyx, abcdefeghi (efe), or even aaa.

For example:

- qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and a 
  letter that repeats with exactly one letter between them (zxz).
- xxyxx is nice because it has a pair that appears twice and a letter that 
  repeats with one between, even though the letters used by each rule overlap.
- uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a 
  single letter between them.
- ieodomkazucvgmuy is naughty because it has a repeating letter with one 
  between (odo), but no pair that appears twice.

How many strings are nice under these new rules?

*/

