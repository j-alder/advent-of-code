use crate::util::format_soln_string;

enum Play {
    Rock,
    Paper,
    Scissors,
    Unknown,
}

enum Suggestion {
    Win,
    Lose,
    Draw,
    Unknown,
}

fn decrypt_play(ep: &str) -> Play {
    match ep {
        "A" => Play::Rock,
        "X" => Play::Rock,
        "B" => Play::Paper,
        "Y" => Play::Paper,
        "C" => Play::Scissors,
        "Z" => Play::Scissors,
        _ => Play::Unknown,
    }
}

fn points_for_play(p: &Play) -> i32 {
    match p {
        Play::Rock => 1,
        Play::Paper => 2,
        Play::Scissors => 3,
        Play::Unknown => 0,
    }
}

fn points_for_suggestion(s: &Suggestion) -> i32 {
    match s {
        Suggestion::Win => 6,
        Suggestion::Draw => 3,
        _ => 0,
    }
}

fn determine_win(o: &str, p: &str) -> i32 {
    let plr = decrypt_play(p);
    let opp = decrypt_play(o);
    let mut total = points_for_play(&plr);
    match plr {
        Play::Paper => match opp {
            Play::Rock => total += 6,
            Play::Paper => total += 3,
            _ => (),
        },
        Play::Scissors => match opp {
            Play::Paper => total += 6,
            Play::Scissors => total += 3,
            _ => (),
        },
        Play::Rock => match opp {
            Play::Scissors => total += 6,
            Play::Rock => total += 3,
            _ => (),
        },
        Play::Unknown => (),
    }
    return total;
}

fn decrypt_suggestion(s: &str) -> Suggestion {
    match s {
        "X" => Suggestion::Lose,
        "Y" => Suggestion::Draw,
        "Z" => Suggestion::Win,
        _ => Suggestion::Unknown,
    }
}

fn determine_suggestion_points(o: &str, s: &str) -> i32 {
    let opp = decrypt_play(o);
    let sug = decrypt_suggestion(s);
    let plr = match sug {
        Suggestion::Draw => opp,
        Suggestion::Lose => match opp {
            Play::Paper => Play::Rock,
            Play::Rock => Play::Scissors,
            Play::Scissors => Play::Paper,
            Play::Unknown => Play::Unknown,
        },
        Suggestion::Win => match opp {
            Play::Paper => Play::Scissors,
            Play::Rock => Play::Paper,
            Play::Scissors => Play::Rock,
            Play::Unknown => Play::Unknown,
        },
        Suggestion::Unknown => Play::Unknown,
    };
    return points_for_play(&plr) + points_for_suggestion(&sug);
}

pub fn soln(input: String) -> String {
    let mut total_one: i32 = 0;
    for c in input.split('\n').into_iter() {
        let round: Vec<&str> = c.split(' ').collect();
        if round.len() > 1 {
            total_one += determine_win(round[0], round[1]);
        }
    }

    let mut total_two: i32 = 0;

    for c in input.split('\n').into_iter() {
        let round: Vec<&str> = c.split(' ').collect();
        if round.len() > 1 {
            total_two += determine_suggestion_points(round[0], round[1]);
        }
    }

    return format_soln_string(total_one.to_string(), total_two.to_string());
}
