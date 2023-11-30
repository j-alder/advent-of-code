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

/*
--- Day 2: Rock Paper Scissors ---

The Elves begin to set up camp on the beach. To decide whose tent gets to be closest to the snack storage,
a giant Rock Paper Scissors tournament is already in progress.

Rock Paper Scissors is a game between two players. Each game contains many rounds; in each round, the
players each simultaneously choose one of Rock, Paper, or Scissors using a hand shape. Then, a winner for
that round is selected: Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock. If both players
choose the same shape, the round instead ends in a draw.

Appreciative of your help yesterday, one Elf gives you an encrypted strategy guide (your puzzle input) that
they say will be sure to help you win. "The first column is what your opponent is going to play: A for Rock,
B for Paper, and C for Scissors. The second column--" Suddenly, the Elf is called away to help with someone's
tent.

The second column, you reason, must be what you should play in response: X for Rock, Y for Paper, and Z for
Scissors. Winning every time would be suspicious, so the responses must have been carefully chosen.

The winner of the whole tournament is the player with the highest score. Your total score is the sum of your
scores for each round. The score for a single round is the score for the shape you selected (1 for Rock, 2
for Paper, and 3 for Scissors) plus the score for the outcome of the round (0 if you lost, 3 if the round was
a draw, and 6 if you won).

Since you can't be sure if the Elf is trying to help you or trick you, you should calculate the score you
would get if you were to follow the strategy guide.

For example, suppose you were given the following strategy guide:

A Y
B X
C Z

This strategy guide predicts and recommends the following:

    In the first round, your opponent will choose Rock (A), and you should choose Paper (Y). This ends
        in a win for you with a score of 8 (2 because you chose Paper + 6 because you won).
    In the second round, your opponent will choose Paper (B), and you should choose Rock (X). This ends
        in a loss for you with a score of 1 (1 + 0).
    The third round is a draw with both players choosing Scissors, giving you a score of 3 + 3 = 6.

In this example, if you were to follow the strategy guide, you would get a total score of 15 (8 + 1 + 6).

What would your total score be if everything goes exactly according to your strategy guide?

--- Part Two ---

The Elf finishes helping with the tent and sneaks back over to you. "Anyway, the second column says how
the round needs to end: X means you need to lose, Y means you need to end the round in a draw, and Z means
you need to win. Good luck!"

The total score is still calculated in the same way, but now you need to figure out what shape to choose
so the round ends as indicated. The example above now goes like this:

    In the first round, your opponent will choose Rock (A), and you need the round to end in a draw (Y),
        so you also choose Rock. This gives you a score of 1 + 3 = 4.
    In the second round, your opponent will choose Paper (B), and you choose Rock so you lose (X) with a
        score of 1 + 0 = 1.
    In the third round, you will defeat your opponent's Scissors with Rock for a score of 1 + 6 = 7.

Now that you're correctly decrypting the ultra top secret strategy guide, you would get a total score of 12.

Following the Elf's instructions for the second column, what would your total score be if everything goes
exactly according to your strategy guide?
 */
