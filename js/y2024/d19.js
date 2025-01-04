const { fmtAnsWithRuntime } = require("../util.js");

function canBeMade(design, patterns, maxPatternLength, memory = new Map()) {
  if (design.length == 0) {
    return true;
  }
  if (memory.has(design)) {
    return memory.get(design);
  }
  for (let i = 1; i <= Math.min(design.length, maxPatternLength); i++) {
    if (
      patterns.has(design.substring(0, i)) &&
      canBeMade(design.substring(i), patterns, maxPatternLength, memory)
    ) {
      memory.set(design, true);
      return true;
    }
  }
  memory.set(design, false);
  return false;
}

function partOne(patterns, designs, maxPatternLength) {
  let possible = 0;
  for (const design of designs) {
    possible += canBeMade(design, patterns, maxPatternLength) ? 1 : 0;
  }
  return possible;
}

function findAllCombinations(design, patterns, maxPatternLength, memory = new Map()) {
  if (design.length == 0) {
    return 1;
  }
  if (memory.has(design)) {
    return memory.get(design);
  }

  let count = 0;
  for (let i = 0; i <= Math.min(design.length, maxPatternLength); i++) {
    const ss = design.substring(0, i);
    if (patterns.has(ss)) {
      count += findAllCombinations(
        design.substring(i),
        patterns,
        maxPatternLength,
        memory
      );
    }
  }

  memory.set(design, count);
  return count;
}

function partTwo(patterns, designs, maxPatternLength) {
  let total = 0;
  for (const design of designs) {
    total += findAllCombinations(design, patterns, maxPatternLength);
  }
  return total;
}

function soln(rawInput) {
  const [rawPatterns, rawDesigns] = rawInput.split("\n\n");
  const patterns = new Set(rawPatterns.split(", "));
  const maxPatternLength = [...patterns].reduce(
    (m, p) => (p.length > m ? p.length : m),
    0
  );
  const designs = rawDesigns.split("\n");
  fmtAnsWithRuntime(
    () => partOne(patterns, designs, maxPatternLength),
    () => partTwo(patterns, designs, maxPatternLength)
  );
}

module.exports = { soln };
