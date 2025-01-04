const { fmtAnsWithRuntime } = require('../util.js');

const dirs = [
  [0, -1], // s
  [-1, 0], // w
  [0, 1], // n
  [1, 0] // e
];

/**
 * @param {number[]} currLoc
 * @param {number} dir
 */
function changeLoc(currLoc, dir) {
  currLoc[0] += dirs[dir][0];
  currLoc[1] += dirs[dir][1];
}

/**
 * @param {number[]} currLoc 
 * @param {string} dir 
 * @param {Set<string>} houses
 */
function deliverPresent(currLoc, dir, houses) {
  switch (dir) {
    case "v":
      changeLoc(currLoc, 0);
      houses.add(currLoc.join(","));
      break;
    case "<":
      changeLoc(currLoc, 1);
      houses.add(currLoc.join(","));
      break;
    case "^":
      changeLoc(currLoc, 2);
      houses.add(currLoc.join(","));
      break;
    case ">":
      changeLoc(currLoc, 3);
      houses.add(currLoc.join(","));
      break;
  }
}

/**
 * @param {string} rawInput
 */
function partOne(input) {
  const houses = new Set(["0,0"]);
  const currLoc = [0, 0];
  for (let a = 0; a < input.length; a++) {
    deliverPresent(currLoc, input[a], houses);
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
    deliverPresent(currLoc, input[a], houses);
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
