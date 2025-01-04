const { fmtAnsWithRuntime } = require('../util.js');

const dirs = [
  [0, -1], // s
  [-1, 0], // w
  [0, 1], // n
  [1, 0] // e
];

/**
 * @param {string} rawInput
 */
function partOne(input) {
  const houses = new Set(["0,0"]);
  const currLoc = [0, 0];
  for (let a = 0; a < input.length; a++) {
    switch (input[a]) {
      case "v":
        currLoc[0] += dirs[0][0];
        currLoc[1] += dirs[0][1];
        houses.add(currLoc.join(","));
        break;
      case "<":
        currLoc[0] += dirs[1][0];
        currLoc[1] += dirs[1][1];
        houses.add(currLoc.join(","));
        break;
      case "^":
        currLoc[0] += dirs[2][0];
        currLoc[1] += dirs[2][1];
        houses.add(currLoc.join(","));
        break;
      case ">":
        currLoc[0] += dirs[3][0];
        currLoc[1] += dirs[3][1];
        houses.add(currLoc.join(","));
        break;
    }
  }
  return houses.size;
}

/**
 * @param {string} rawInput
 */
function partTwo(input) {
  const houses = new Set(["0,0"]);
  const santa = [0, 0];
  const roboSanta = [0, 0];
  for (let a = 0; a < input.length; a++) {
    const currLoc = a % 2 == 0 ? santa : roboSanta;
    switch (input[a]) {
      case "v":
        currLoc[0] += dirs[0][0];
        currLoc[1] += dirs[0][1];
        houses.add(currLoc.join(","));
        break;
      case "<":
        currLoc[0] += dirs[1][0];
        currLoc[1] += dirs[1][1];
        houses.add(currLoc.join(","));
        break;
      case "^":
        currLoc[0] += dirs[2][0];
        currLoc[1] += dirs[2][1];
        houses.add(currLoc.join(","));
        break;
      case ">":
        currLoc[0] += dirs[3][0];
        currLoc[1] += dirs[3][1];
        houses.add(currLoc.join(","));
        break;
    }
  }
  return houses.size;
}

function soln(rawInput) {
  fmtAnsWithRuntime(
    () => partOne(rawInput),
    () => partTwo(rawInput)
  );
}

module.exports = { soln };
