const { fmtAnsWithRuntime } = require("../util.js");

function partOne(input) {
  return input.reduce((count, line, lineIdx, lines) => {
    let localCount = 0;
    for (let charIdx = 0; charIdx < line.length; charIdx++) {
      if (line[charIdx] == "X") {
        // horizontal XMAS
        if (
          line[charIdx + 1] == "M" &&
          line[charIdx + 2] == "A" &&
          line[charIdx + 3] == "S"
        ) {
          localCount++;
        }
        // vertical XMAS
        if (
          lines[lineIdx + 1]?.[charIdx] == "M" &&
          lines[lineIdx + 2]?.[charIdx] == "A" &&
          lines[lineIdx + 3]?.[charIdx] == "S"
        ) {
          localCount++;
        }
        // right diagonal XMAS
        if (
          lines[lineIdx + 1]?.[charIdx + 1] == "M" &&
          lines[lineIdx + 2]?.[charIdx + 2] == "A" &&
          lines[lineIdx + 3]?.[charIdx + 3] == "S"
        ) {
          localCount++;
        }
        // left diagonal XMAS
        if (
          lines[lineIdx + 1]?.[charIdx - 1] == "M" &&
          lines[lineIdx + 2]?.[charIdx - 2] == "A" &&
          lines[lineIdx + 3]?.[charIdx - 3] == "S"
        ) {
          localCount++;
        }
      }
      if (line[charIdx] == "S") {
        // horizontal SAMX
        if (
          line[charIdx + 1] == "A" &&
          line[charIdx + 2] == "M" &&
          line[charIdx + 3] == "X"
        ) {
          localCount++;
        }
        // vertical SAMX
        if (
          lines[lineIdx + 1]?.[charIdx] == "A" &&
          lines[lineIdx + 2]?.[charIdx] == "M" &&
          lines[lineIdx + 3]?.[charIdx] == "X"
        ) {
          localCount++;
        }
        // right diagonal SAMX
        if (
          lines[lineIdx + 1]?.[charIdx + 1] == "A" &&
          lines[lineIdx + 2]?.[charIdx + 2] == "M" &&
          lines[lineIdx + 3]?.[charIdx + 3] == "X"
        ) {
          localCount++;
        }
        // left diagonal SAMX
        if (
          lines[lineIdx + 1]?.[charIdx - 1] == "A" &&
          lines[lineIdx + 2]?.[charIdx - 2] == "M" &&
          lines[lineIdx + 3]?.[charIdx - 3] == "X"
        ) {
          localCount++;
        }
      }
    }
    return count + localCount;
  }, 0);
}

function partTwo(input) {
  return input.reduce((count, line, lineIdx, lines) => {
    let localCount = 0;
    for (let charIdx = 0; charIdx < line.length; charIdx++) {
      if (line[charIdx] == "A") {
        if (
          /*
        M S
         A
        M S
        */
          (lines[lineIdx - 1]?.[charIdx - 1] == "M" &&
            lines[lineIdx - 1]?.[charIdx + 1] == "S" &&
            lines[lineIdx + 1]?.[charIdx - 1] == "M" &&
            lines[lineIdx + 1]?.[charIdx + 1] == "S") ||
          /*
        M M
         A
        S S
        */
          (lines[lineIdx - 1]?.[charIdx - 1] == "M" &&
            lines[lineIdx - 1]?.[charIdx + 1] == "M" &&
            lines[lineIdx + 1]?.[charIdx - 1] == "S" &&
            lines[lineIdx + 1]?.[charIdx + 1] == "S") ||
          /*
        S M
         A
        S M
        */
          (lines[lineIdx - 1]?.[charIdx - 1] == "S" &&
            lines[lineIdx - 1]?.[charIdx + 1] == "M" &&
            lines[lineIdx + 1]?.[charIdx - 1] == "S" &&
            lines[lineIdx + 1]?.[charIdx + 1] == "M") ||
          /*
        S S
         A
        M M
        */
          (lines[lineIdx - 1]?.[charIdx - 1] == "S" &&
            lines[lineIdx - 1]?.[charIdx + 1] == "S" &&
            lines[lineIdx + 1]?.[charIdx - 1] == "M" &&
            lines[lineIdx + 1]?.[charIdx + 1] == "M")
        ) {
          localCount++;
        }
      }
    }
    return count + localCount;
  }, 0);
}

function soln(rawInput) {
  const input = rawInput.split("\n").map((ln) => ln.split(""));
  fmtAnsWithRuntime(
    () => partOne(input),
    () => partTwo(input)
  );
}

module.exports = { soln };

/*
--- Day 4: Ceres Search ---
"Looks like the Chief's not here. Next!" One of The Historians pulls out a device 
and pushes the only button on it. After a brief flash, you recognize the interior 
of the Ceres monitoring station!

As the search for the Chief continues, a small Elf who lives on the station tugs 
on your shirt; she'd like to know if you could help her with her word search 
(your puzzle input). She only has to find one word: XMAS.

This word search allows words to be horizontal, vertical, diagonal, written 
backwards, or even overlapping other words. It's a little unusual, though, as you 
don't merely need to find one instance of XMAS - you need to find all of them. 
Here are a few ways XMAS might appear, where irrelevant characters have been 
replaced with .:


..X...
.SAMX.
.A..A.
XMAS.S
.X....
The actual word search will be full of letters instead. For example:

MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
In this word search, XMAS occurs a total of 18 times; here's the same word search 
again, but where letters not involved in any XMAS have been replaced with .:

....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX
Take a look at the little Elf's word search. How many times does XMAS appear?

--- Part Two ---
The Elf looks quizzically at you. Did you misunderstand the assignment?

Looking for the instructions, you flip over the word search to find that this 
isn't actually an XMAS puzzle; it's an X-MAS puzzle in which you're supposed to 
find two MAS in the shape of an X. One way to achieve that is like this:

M.S
.A.
M.S
Irrelevant characters have again been replaced with . in the above diagram. Within 
the X, each MAS can be written forwards or backwards.

Here's the same example from before, but this time all of the X-MASes have been 
kept instead:

.M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........
In this example, an X-MAS appears 9 times.

Flip the word search from the instructions back over to the word search side and 
try again. How many times does an X-MAS appear?
*/
