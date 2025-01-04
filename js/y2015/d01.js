const { fmtAnsWithRuntime } = require('../util.js');

function partOne(input) {
  let floor = 0;
  for (let a = 0; a < input.length; a++) {
    if (input[a] === "(") {
      floor++;
    }
    if (input[a] === ")") {
      floor--;
    }
  }
  return floor;
}

function partTwo(input) {
  let floor = 0;
  for (let a = 0; a < input.length; a++) {
    if (input[a] === "(") {
      floor++;
    }
    if (input[a] === ")") {
      floor--;
    }
    if (floor < 0) return a + 1;
  }
}

function soln(rawInput) {
  fmtAnsWithRuntime(
    () => partOne(rawInput),
    () => partTwo(rawInput)
  );
}

module.exports = { soln };
