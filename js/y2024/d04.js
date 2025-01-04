const { fmtAnsWithRuntime, getNeighbors } = require("../util.js");

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
        const neighbors = getNeighbors(lineIdx, charIdx, lines, {
          nw: [-1, -1],
          ne: [-1, 1],
          sw: [1, -1],
          se: [1, 1],
        });
        if (
          /*
        M S
         A
        M S
        */
          (neighbors.nw == "M" &&
            neighbors.ne == "S" &&
            neighbors.sw == "M" &&
            neighbors.se == "S") ||
          /*
        M M
         A
        S S
        */
          (neighbors.nw == "M" &&
            neighbors.ne == "M" &&
            neighbors.sw == "S" &&
            neighbors.se == "S") ||
          /*
        S M
         A
        S M
        */
          (neighbors.nw == "S" &&
            neighbors.ne == "M" &&
            neighbors.sw == "S" &&
            neighbors.se == "M") ||
          /*
        S S
         A
        M M
        */
          (neighbors.nw == "S" &&
            neighbors.ne == "S" &&
            neighbors.sw == "M" &&
            neighbors.se == "M")
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
