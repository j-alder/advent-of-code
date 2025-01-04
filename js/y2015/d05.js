const { fmtAnsWithRuntime } = require("../util.js");

function partOne(input) {
  return input.reduce((total, word) => {
    if (
      [...word.matchAll(/[aeiou]/g)].length > 2 &&
      word.split("").some((it, idx) => it === word[idx + 1]) &&
      [...word.matchAll(/(ab)|(cd)|(pq)|(xy)/g)].length === 0
    ) {
      return total + 1;
    } else {
      return total;
    }
  }, 0);
}

function isNice(word) {
  const letters = word.split("");
  return (
    letters.some((letter, idx, arr) => letter === arr[idx + 2]) &&
    letters.some((letter, idx, arr) => {
      if (arr[idx + 1] != undefined) {
        return (
          [...word.matchAll(new RegExp(letter + arr[idx + 1], "g"))].length > 1
        );
      }
      return false;
    })
  );
}

function partTwo(input) {
  return input.reduce((total, word) => {
    if (isNice(word)) return total + 1;
    else return total;
  }, 0);
}

function soln(rawInput) {
  const input = rawInput.split("\n");
  fmtAnsWithRuntime(
    () => partOne(input),
    () => partTwo(input)
  );
}

module.exports = { soln };
