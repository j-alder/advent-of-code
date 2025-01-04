const { fmtAnsWithRuntime, betweenInc } = require("../util.js");

const dirs = [
  [0, 1, 0], // e
  [1, 0, 1], // s
  [0, -1, 2], // w
  [-1, 0, 3], // n
];

function getShortestDistance(matrix, [sx, sy], [ex, ey]) {
  const queue = [[sx, sy, 0, 0]];
  const visited = new Set();

  while (queue.length) {
    queue.sort((a, b) => a[3] - b[3]);

    const [x, y, d, score] = queue.shift();
    const key = `${x},${y},${d}`;

    if (x === ex && y === ey) {
      return score;
    }

    if (visited.has(key)) {
      continue;
    }

    visited.add(key);

    dirs
      .filter(
        ([ny, nx]) =>
          betweenInc(x + nx, 0, matrix[0].length - 1) &&
          betweenInc(y + ny, 0, matrix.length - 1)
      )
      .forEach(([ny, nx, dir]) => {
        if (matrix[x + nx][y + ny] !== "#") {
          queue.push([x + nx, y + ny, dir, score + 1]);
        }
      });
  }
  return -1;
}

function dropBytes(numBytes, gridSize, bytes) {
  const grid = Array.from({ length: gridSize }, () =>
    Array.from({ length: gridSize }, () => ".")
  );
  for (let i = 0; i < numBytes; i++) {
    grid[bytes[i][0]][bytes[i][1]] = "#";
  }
  return grid;
}

function partOne(bytes) {
  const grid = dropBytes(1024, 71, bytes);
  return getShortestDistance(grid, [0, 0], [70, 70]);
}

function partTwo(bytes) {
  let left = 0;
  let right = bytes.length - 1;
  let mid;
  while (left < right) {
    mid = Math.floor((left + right) / 2);
    if (getShortestDistance(dropBytes(mid, 71, bytes), [0, 0], [70, 70]) == -1) {
      right = mid - 1;
    } else {
      left = mid + 1;
    }
  }
  return [bytes[mid][1], bytes[mid][0]];
}

function soln(rawInput) {
  const input = rawInput
    .split("\n")
    .map((ln) => ln.split(","))
    .map(([y, x]) => [Number(x), Number(y)]);
  fmtAnsWithRuntime(
    () => partOne(input),
    () => partTwo(input)
  );
}

module.exports = { soln };
