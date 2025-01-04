const { fmtAnsWithRuntime } = require('../util.js');

function partOne(input) {
  const mulRegex = /mul\(\d+,\d+\)/g;
  return input.match(mulRegex).reduce((acc, mul) => {
    const [a, b] = mul.match(/\d+/g).map(Number);
    return acc + a * b;
  }, 0);
}

// less readable, (very) slightly more performant
// use capturing groups to get operands without using a second match
const partOneOpt = (input) =>
  [...input.matchAll(/mul\((\d+),(\d+)\)/g)]
    .reduce((acc, mul) => acc + mul[1] * mul[2], 0);

function partTwo(input) {
  const enabledMulRegex = /(do\(\))|(don't\(\))|(mul\(\d+,\d+\))/g;
  let enabled = true;
  return input.match(enabledMulRegex).reduce((acc, inst) => {
    if (inst == "do()") {
      enabled = true;
    }
    if (inst == "don't()") {
      enabled = false;
    }
    if (enabled && inst.startsWith("mul")) {
      const [a, b] = inst.match(/\d+/g).map(Number);
      return acc + a * b;
    }
    return acc;
  }, 0);
}

// less readable, (very) slightly more performant
// use capturing groups to get operations and operands without using sequential matches
const partTwoOpt = (input) =>
  [...input.matchAll(/(?:do\(\))|(?:don't\(\))|(?:mul\((\d+),(\d+)\))/g)]
    .reduce((acc, inst) => {
      if (inst[0] == "do()") {
        return [acc[0], true];
      }
      if (inst[0] == "don't()") {
        return [acc[0], false];
      }
      if (acc[1]) {
        return [acc[0] + Number(inst[1]) * Number(inst[2]), acc[1]];
      }
      return acc;
    }, [0, true])[0]

function soln(rawInput) {
  fmtAnsWithRuntime(
    () => partOneOpt(rawInput),
    () => partTwoOpt(rawInput)
  );
}

module.exports = { soln };
