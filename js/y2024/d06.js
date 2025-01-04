const { fmtAnsWithRuntime, coordsOf } = require("../util.js");

const gridValue = (grid, x, y) => grid[x]?.[y];

const dirs = [
  [-1, 0],
  [0, 1],
  [1, 0],
  [0, -1],
];

const mutCur = (curr, dir) => [curr[0] + dirs[dir][0], curr[1] + dirs[dir][1]];

function traverse([startX, startY], grid) {
  let dir = 0;
  let curr = [startX, startY];
  const path = [];
  while (gridValue(grid, curr[0], curr[1]) != null) {
    path.push(curr);
    const [x, y] = mutCur(curr, dir);
    if (gridValue(grid, x, y) == "#") {
      dir++;
      dir %= 4;
    } else {
      curr = [x, y];
    }
  }
  return path;
}

function partOne(grid) {
  let start = coordsOf("^", grid);
  const path = traverse(start, grid);
  return new Set(path.map(([x, y]) => `${x},${y}`)).size;
}

function producesLoop([startX, startY], grid) {
  let dir = 0;
  let curr = [startX, startY];
  const visited = new Set();
  while (gridValue(grid, curr[0], curr[1]) != null) {
    const visitedStr = `${curr[0]},${curr[1]},${dirs[dir][0]},${dirs[dir][1]}}`;
    if (visited.has(visitedStr)) {
      return true;
    }
    visited.add(visitedStr);
    const [x, y] = mutCur(curr, dir);
    if (gridValue(grid, x, y) == "#") {
      dir++;
      dir %= 4;
    } else {
      curr = [x, y];
    }
  }
  return false;
}

function partTwo(grid) {
  let start = coordsOf("^", grid);
  const path = traverse(start, grid);

  const uniqueNodeStrs = [...new Set(path.map(([x, y]) => `${x},${y}`))];
  const uniqueNodes = uniqueNodeStrs
    .map((nodeStr) => nodeStr.split(",").map(Number))
    .splice(1, uniqueNodeStrs.length);

  let count = 0;

  for ([x, y] of uniqueNodes) {
    const prev = grid[x][y];
    grid[x][y] = "#";
    if (producesLoop(start, grid)) {
      count++;
    }
    grid[x][y] = prev;
  }

  return count;
}

function soln(rawInput) {
  const input = rawInput.split("\n").map((ln) => ln.split(""));
  fmtAnsWithRuntime(
    () => partOne(input),
    () => partTwo(input)
  );
}

module.exports = { soln };
