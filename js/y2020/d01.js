const { fmtAnsWithRuntime } = require('../util.js');

function partOne(numbers) {
  const set = new Set(numbers);
  for (const num of numbers) {
    const diff = 2020 - num;
    if (set.has(diff)) {
      return num * diff;
    }
  }
}

function partTwo(numbers) {
  const set = new Set(numbers);
  for (const n of set) {
    for (const m of set) {
      const diff = 2020 - n - m;
      if (set.has(diff)) {
        return diff * n * m;
      }
    }
  }
}

function soln(rawInput) {
  const numbers = rawInput.split('\n').filter(s => s !== '').map(Number);
  fmtAnsWithRuntime(() => partOne(numbers), () => partTwo(numbers));
}

module.exports = { soln };
