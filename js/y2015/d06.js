const { fmtAnsWithRuntime, sum, print2dArray } = require("../util.js");

/**
 * @param {string} rawInput
 */
function parseInput(rawInput) {
  return rawInput.split("\n").map((ln) => {
    const command = ln.match(/(on|off|toggle)/)?.[0];
    const [x1, y1, x2, y2] = [...ln.matchAll(/\d+/g)].map((it) =>
      Number(it[0])
    );
    return [command, [x1, y1], [x2, y2]];
  });
}

function setGrid(fn, [x1, y1], [x2, y2], grid) {
  let cy = y1;
  while (cy <= y2) {
    let cx = x1;
    while (cx <= x2) {
      grid[cx][cy] = fn(grid[cx][cy]);
      cx++;
    }
    cy++;
  }
}

/**
 * @param {string} rawInput
 */
function partOne(rawInput) {
  const inst = parseInput(rawInput);
  const grid = Array.from({ length: 1000 }, () =>
    Array.from({ length: 1000 }, () => 0)
  );
  for (const [cmd, start, end] of inst) {
    let fn;
    switch (cmd) {
      case "on":
        fn = () => 1;
        break;
      case "off":
        fn = () => 0;
        break;
      case "toggle":
        fn = (v) => 1 - v;
        break;
    }
    setGrid(fn, start, end, grid);
  }
  let total = 0;
  for (const subArr of grid) {
    total += sum(subArr);
  }
  return total;
}

/**
 * @param {string} rawInput
 */
function partTwo(rawInput) {
  const inst = parseInput(rawInput);
  const grid = Array.from({ length: 1000 }, () =>
    Array.from({ length: 1000 }, () => 0)
  );
  for (const [cmd, start, end] of inst) {
    let fn;
    switch (cmd) {
      case "on":
        fn = (v) => v + 1;
        break;
      case "off":
        fn = (v) => Math.max(0, v - 1);
        break;
      case "toggle":
        fn = (v) => v + 2;
        break;
    }
    setGrid(fn, start, end, grid);
  }
  let total = 0;
  for (const subArr of grid) {
    total += sum(subArr);
  }
  return total;
}

function soln(rawInput) {
  fmtAnsWithRuntime(
    () => partOne(rawInput),
    () => partTwo(rawInput)
  );
}

module.exports = { soln };
