const { fmtAnsWithRuntime } = require("../util.js");

function permutations(permutationLength, alphabet) {
  const result = [];

  function generate(current = "") {
    if (current.length === permutationLength) {
      result.push(current);
      return;
    }

    for (const a of alphabet) {
      generate(current + a);
    }
  }

  generate();
  return result;
}

function calc(operands, operators) {
  let n = operands[0];
  for (let i = 1; i < operands.length; i++) {
    if (operators[i - 1] == "+") n += operands[i];
    if (operators[i - 1] == "*") n *= operands[i];
    if (operators[i - 1] == "|") n = Number(`${n}${operands[i]}`);
  }
  return n;
}

function canMakeTotal(total, operands, opPerms) {
  for (const operators of opPerms) {
    if (calc(operands, operators) === total) return true;
  }
  return false;
}

function partOne(input) {
  const permMemo = new Map();
  return input.reduce((count, [total, operands]) => {
    let opPerms;
    if (permMemo.has(operands.length - 1)) {
      opPerms = permMemo.get(operands.length - 1);
    } else {
      opPerms = permutations(operands.length - 1, ["*", "+"]);
      permMemo.set(operands.length - 1, opPerms);
    }
    if (canMakeTotal(total, operands, opPerms)) return count + total;
    return count;
  }, 0);
}

function partTwo(input) {
  const permMemo = new Map();
  return input.reduce((count, [total, operands]) => {
    if (permMemo.has(operands.length - 1)) {
      opPerms = permMemo.get(operands.length - 1);
    } else {
      opPerms = permutations(operands.length - 1, ["*", "+", "|"]);
      permMemo.set(operands.length - 1, opPerms);
    }
    if (canMakeTotal(total, operands, opPerms)) return count + total;
    return count;
  }, 0);
}

function soln(rawInput) {
  const input = rawInput
    .split("\n")
    .map((ln) => ln.split(": "))
    .map(([rt, ro]) => [Number(rt), ro.split(" ").map(Number)]);
  fmtAnsWithRuntime(
    () => partOne(input),
    () => partTwo(input)
  );
}

module.exports = { soln };
