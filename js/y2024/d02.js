const { fmtAnsWithRuntime, between } = require("../util.js");

const validDiffs = (diffs)=> 
  diffs[0] < 0
    ? diffs.every((diff) => diff < 0 && between(diff, -4, 0))
    : diffs.every((diff) => diff > 0 && between(diff, 0, 4));

const genDiffs = (report, skipIndex = -1) =>
  report
    .filter((_, idx) => idx !== skipIndex)
    .reduce((diffs, val, idx, filteredReport) => {
      if (idx >= filteredReport.length - 1) return diffs;
      return [...diffs, filteredReport[idx + 1] - val];
    }, []);

function isSafe(report, skipIndex = -1) {
  const diffs = genDiffs(report, skipIndex);
  if (!validDiffs(diffs)) return false;
  return true;
}

function partOne(input) {
  return input.reduce((acc, report) => {
    if (isSafe(report)) return acc + 1;
    return acc;
  }, 0);
}

function partTwo(input) {
  return input.reduce((acc, report) => {
    let safe = false;
    if (isSafe(report)) safe = true;
    let i = 0;
    while (!safe && i < report.length) {
      safe = isSafe(report, i);
      i++;
    }
    if (safe) return acc + 1;
    return acc;
  }, 0);
}

function soln(rawInput) {
  const input = rawInput
    .split("\n")
    .map((rawReport) => rawReport.split(" ").map(Number));
  fmtAnsWithRuntime(
    () => partOne(input),
    () => partTwo(input)
  );
}

module.exports = { soln };
