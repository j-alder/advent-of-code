const { fmtAnsWithRuntime, coordsOf } = require("../util.js");

const dirs = [
  [0, 1], // e
  [1, 0], // s
  [0, -1], // w
  [-1, 0], // n
];

function getLowestScore(matrix, [sx, sy], [ex, ey]) {
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

    const [fx, fy] = [x + dirs[d][0], y + dirs[d][1]];

    if (matrix[fx]?.[fy] !== "#") {
      queue.push([fx, fy, d, score + 1]);
    }

    queue.push([x, y, (d + 1) % 4, score + 1000]);
    queue.push([x, y, (d + 3) % 4, score + 1000]);
  }
  return -1;
}

function partOne(matrix) {
  const start = coordsOf("S", matrix);
  const end = coordsOf("E", matrix);
  return getLowestScore(matrix, start, end);
}

function getAllPathsWithScore(matrix, [sx, sy], [ex, ey], targetScore) {
  const queue = [[sx, sy, 0, 0, [[sx, sy]]]];
  const visited = {};
  const paths = [];

  while (queue.length) {
    const [x, y, d, score, path] = queue.shift();
    const key = `${x},${y},${d}`;

    if (score > targetScore || (visited[key] != null && visited[key] < score)) {
      continue;
    }

    visited[key] = score;

    if (x === ex && y === ey && score === targetScore) {
      paths.push(path);
      continue;
    }

    const [fx, fy] = [x + dirs[d][0], y + dirs[d][1]];

    if (matrix[fx]?.[fy] !== "#") {
      queue.push([fx, fy, d, score + 1, [...path, [fx, fy]]]);
    }

    queue.push([x, y, (d + 1) % 4, score + 1000, [...path]]);
    queue.push([x, y, (d + 3) % 4, score + 1000, [...path]]);
  }

  return paths;
}

function partTwo(matrix) {
  const start = coordsOf("S", matrix);
  const end = coordsOf("E", matrix);
  const lowestScore = getLowestScore(matrix, start, end);
  return getAllPathsWithScore(matrix, start, end, lowestScore).reduce(
    (uniqueSteps, path) => {
      path.forEach((step) => uniqueSteps.add(`${step}`));
      return uniqueSteps;
    },
    new Set()
  ).size;
}

function soln(rawInput) {
  const matrix = rawInput.split("\n").map((ln) => ln.split(""));
  fmtAnsWithRuntime(
    () => partOne(matrix),
    () => partTwo(matrix)
  );
}

module.exports = { soln };
