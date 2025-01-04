const { fmtAnsWithRuntime } = require("../util.js");

/**
 *
 * @param {string} rawInput
 */
function parseInput(rawInput) {
  return rawInput
    .split("\n\n")
    .map((klRaw) => klRaw.split("\n").map((col) => col.split("")))
    .reduce(
      (result, keyOrLock) => {
        if (keyOrLock[0].every((it) => it == "#")) {
          const heights = [0, 0, 0, 0, 0];
          for (let i = 1; i < keyOrLock.length; i++) {
            for (let j = 0; j < keyOrLock[i].length; j++) {
              if (keyOrLock[i][j] == "#") {
                heights[j] += 1;
              }
            }
          }
          result.locks.push(heights);
        } else if (keyOrLock[keyOrLock.length - 1].every((it) => it == "#")) {
          const heights = [0, 0, 0, 0, 0];
          for (let i = keyOrLock.length - 2; i >= 0; i--) {
            for (let j = 0; j < keyOrLock[i].length; j++) {
              if (keyOrLock[i][j] == "#") {
                heights[j] += 1;
              }
            }
          }
          result.keys.push(heights);
        }
        return result;
      },
      { keys: [], locks: [] }
    );
}

function partOne(input) {
  const { keys, locks } = parseInput(input);
  let matches = 0;

  for (const key of keys) {
    for (const lock of locks) {
      let match = true;
      for (a = 0; a < key.length; a++) {
        if (key[a] + lock[a] > 5) {
          match = false;
          break;
        }
      }
      if (match) {
        matches++;
      }
    }
  }
  return matches;
}

function partTwo(input) {}

function soln(rawInput) {
  fmtAnsWithRuntime(
    () => partOne(rawInput),
    () => partTwo(rawInput)
  );
}

module.exports = { soln };
