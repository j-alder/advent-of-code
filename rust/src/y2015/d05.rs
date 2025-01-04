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
