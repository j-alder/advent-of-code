const { fmtAnsWithRuntime } = require("../util.js");

/**
 * @param {string} rawInput
 */
function parseInput(rawInput) {
  return [...rawInput.matchAll(/.+\n/g)].map((mtch) =>
    [...mtch[0].matchAll(/\d+/g)].map((it) => Number(it[0]))
  );
}

/**
 * @param {string} rawInput
 */
function partOne(rawInput) {
  const input = parseInput(rawInput);
  let totalSqFt = 0;
  for (const [l, w, h] of input) {
    const surfaceArea = 2 * l * w + 2 * w * h + 2 * h * l;
    const smallestSide = Math.min(l * w, l * h, w * h);
    totalSqFt += surfaceArea + smallestSide;
  }
  return totalSqFt;
}

/**
 * @param {string} rawInput
 */
function partTwo(rawInput) {
  const input = parseInput(rawInput);
  let totalFt = 0;
  for (const dim of input) {
    dim.sort((a, b) => a - b);
    const max = dim.pop();
    totalFt += (dim[0] * 2 + dim[1] * 2) + (dim[0] * dim[1] * max);
  }
  return totalFt;
}

function soln(rawInput) {
  fmtAnsWithRuntime(
    () => partOne(rawInput),
    () => partTwo(rawInput)
  );
}

module.exports = { soln };
