const { fmtAnsWithRuntime } = require("../util.js");

/**
 * Use Cramer's Rule to determine the correct number of A and B button presses
 * to reach the prize.
 * 
 * @param {{a: [number, number], b: [number, number], p: [number, number]}} machine 
 * @param {number?} offset 
 * @returns 
 */
function solve(machine, offset = 0) {
  const prize = machine.p.map((p) => p + offset);
  const determinant = machine.a[0] * machine.b[1] - machine.a[1] * machine.b[0];
  const aPresses =
    (prize[0] * machine.b[1] - prize[1] * machine.b[0]) / determinant;
  const bPresses =
    (prize[1] * machine.a[0] - prize[0] * machine.a[1]) / determinant;
  if (aPresses % 1 != 0 || bPresses % 1 != 0) {
    return 0
  }
  return aPresses * 3 + bPresses;
}

const partOne = (input) =>
  input.reduce((total, machine) => total + solve(machine), 0);

const partTwo = (input) =>
  input.reduce((total, machine) => total + solve(machine, 10_000_000_000_000), 0);

function soln(rawInput) {
  const input = rawInput
    .split("\n\n")
    .map((m) => m.split("\n"))
    .map((i) => ({
      a: [...i[0].matchAll(/\d+/g)].map((it) => Number(it[0])),
      b: [...i[1].matchAll(/\d+/g)].map((it) => Number(it[0])),
      p: [...i[2].matchAll(/\d+/g)].map((it) => Number(it[0])),
    }));
  fmtAnsWithRuntime(
    () => partOne(input),
    () => partTwo(input)
  );
}

module.exports = { soln };
