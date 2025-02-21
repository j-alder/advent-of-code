const { fmtAnsWithRuntime } = require('../util.js');

/**
 * @param {string} rawInput
 * @returns {Map<string, string | number>}
 */
function parseInput(rawInput) {
  const m = new Map();
  rawInput.split("\n")
    .forEach(ln => {
      const [expr, out] = ln.split(" -> ");
      if (isNaN(parseInt(expr))) {
        m.set(out, expr);
      } else {
        m.set(out, parseInt(expr));
      }
    });
  return m;
}

function op(gate, a, b) {
  switch (gate) {
    case "RSHIFT":
      return a >> b;
    case "LSHIFT":
      return a << b;
    case "NOT":
      return ~ a;
    case "OR":
      return a ^ b;
    case "AND":
      return a & b;
    default:
      console.log("encountered unhandled gate", gate);
      return a;
  }
}

/**
 * @param {string} rawInput
 */
function partOne(rawInput) {
  const m = parseInput(rawInput);
  while (typeof m.get('a') === "string") {
    for (const [wire, val] of m) {
      if (typeof val !== "number") {
        // split the val and operate if m.get(a) and m.get(b) are both defined
        const [a, b, c] = val.split(" ");
        console.log(a, b, c);
        if (c == null && typeof m.get(b) === "number") {
          console.log(op(a, b));
          m.set(wire, op(a, b));
        } else if (typeof m.get(a) === "number" && typeof m.get(c) === "number") {
          console.log(op(m.get(a), b, m.get(c)));
          m.set(wire, op(m.get(a), b, m.get(c)));
        }
      }
    }
  }
  return m.get('a');
}

/**
 * @param {string} rawInput
 */
function partTwo(rawInput) {
  const input = parseInput(rawInput);
}

function soln(rawInput) {
  fmtAnsWithRuntime(
    () => partOne(rawInput),
    () => partTwo(rawInput)
  );
}

module.exports = { soln };
