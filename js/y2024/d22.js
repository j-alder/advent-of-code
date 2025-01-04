const { fmtAnsWithRuntime } = require("../util.js");

const mix = (value, secret) => (secret ^ value) >>> 0;

const prune = (secret) => secret % 16777216;

const stepOne = (n) => prune(mix(n * 64, n));

const stepTwo = (n) => prune(mix(Math.floor(n / 32), n));

const stepThree = (n) => prune(mix(n * 2048, n));

const createSecretNumber = (initial) => stepThree(stepTwo(stepOne(initial)));

function partOne(secretNumbers) {
  let total = 0;
  for (const initial of secretNumbers) {
    let result = initial;
    for (let i = 0; i < 2000; i++) {
      result = createSecretNumber(result);
    }
    total += result;
  }
  return total;
}

const onesDigit = (secret) => {
  const str = secret.toString();
  return Number(str.charAt(str.length - 1));
};

function shift(lastFive, ones) {
  if (lastFive.length > 4) {
    lastFive.shift();
  }
  lastFive.push(ones);
}

function diffs(lastFive) {
  const result = [];
  for (let i = 0; i < lastFive.length - 1; i++) {
    result.push(lastFive[i + 1] - lastFive[i]);
  }
  return result;
}

function partTwo(secretNumbers) {
  const x = {};
  for (const initial of secretNumbers) {
    let result = initial;
    const lastFive = [];
    for (let i = 0; i < 2000; i++) {
      result = createSecretNumber(result);
      const ones = onesDigit(result);
      shift(lastFive, ones);
      const diff = diffs(lastFive);
      if (diff.length == 4) {
        const seq = diffs(lastFive).join(",");
        if (x[seq] == null) {
          x[seq] = {
            [initial]: ones,
          };
        } else if (x[seq]?.[initial] == null) {
          x[seq][initial] = ones;
        }
      }
    }
  }
  let maxTotal = 0;
  for (const occs of Object.values(x)) {
    const total = Object.values(occs).reduce((acc, ones) => acc + ones, 0);
    if (total > maxTotal) maxTotal = total;
  }
  return maxTotal;
}

function soln(rawInput) {
  const input = rawInput.split("\n").map(Number);
  fmtAnsWithRuntime(
    () => partOne(input),
    () => partTwo(input)
  );
}

module.exports = { soln };
