const { fmtAnsWithRuntime } = require("../util.js");

function partOne(input) {
  // refactor: use an integer for current button.
  // U subtracts 3
  // D adds 3
  // R adds 1
  // L subtracts 1

  const getButton = ([y, x]) =>
    [
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 9],
    ][y][x];

  let currPos = [1, 1];
  return input
    .filter((it) => it !== "")
    .map((ln) =>
      ln.split("").reduce((ans, dir) => {
        switch (dir) {
          case "U":
            if (currPos[0] - 1 >= 0) {
              currPos = [currPos[0] - 1, currPos[1]];
            }
            break;
          case "D":
            if (currPos[0] + 1 <= 2) {
              currPos = [currPos[0] + 1, currPos[1]];
            }
            break;
          case "L":
            if (currPos[1] - 1 >= 0) {
              currPos = [currPos[0], currPos[1] - 1];
            }
            break;
          case "R":
            if (currPos[1] + 1 <= 2) {
              currPos = [currPos[0], currPos[1] + 1];
            }
            break;
          default:
            break;
        }
        ans = getButton(currPos);
        return ans;
      }, 0)
    )
    .join("");
}

function partTwo(input) {
  const getButton = ([y, x]) => {
    try {
      return [["1"], ["2", "3", "4"], ["5", "6", "7", "8", "9"], ["A", "B", "C"], ["D"]][y][x];
    } catch {
      return false;
    }
  };
  let currPos = [1, 1];
  return input
    .filter((it) => it !== "")
    .map((ln) =>
      ln.split("").reduce((ans, dir) => {
        switch (dir) {
          case "U":
            if (getButton([currPos[0] - 1, currPos[1]])) {
              currPos = [currPos[0] - 1, currPos[1]];
            }
            break;
          case "D":
            if (getButton([currPos[0] + 1, currPos[1]])) {
              currPos = [currPos[0] + 1, currPos[1]];
            }
            break;
          case "L":
            if (getButton([currPos[0], currPos[1] - 1])) {
              currPos = [currPos[0], currPos[1] - 1];
            }
            break;
          case "R":
            if (getButton([currPos[0], currPos[1] + 1])) {
              currPos = [currPos[0], currPos[1] + 1];
            }
            break;
          default:
            break;
        }
        ans = getButton(currPos);
        return ans;
      }, "1")
    )
    .join("");
}

function soln(rawInput) {
  const input = rawInput.split("\n");
  fmtAnsWithRuntime(
    () => partOne(input),
    () => partTwo(input)
  );
}

module.exports = { soln };
