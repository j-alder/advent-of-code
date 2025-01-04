const { fmtAnsWithRuntime, coordsOf, getNeighborsWithCoordinates, print2dArray, sum } = require('../util.js');

const dirs = [
  [0, 1],
  [1, 0],
  [0, -1],
  [-1, 0]
];

const clipDirs = [
  [[0, 1], [0, 2]],
  [[1, 0], [2, 0]],
  [[0, -1], [0, -2]],
  [[-1, 0], [-2, 0]]
];

function getDistances(matrix) {
  const [sx, sy] = coordsOf("S", matrix);
  let [cx, cy] = [sx, sy];

  const path = [[cx, cy]];
  const visited = new Set();
  let dist = 1;

  while (matrix[cx][cy] !== "E") {
    for (const d of dirs) {
      const [x, y] = [cx + d[0], cy + d[1]];
      if (!visited.has(`${[x,y]}`) && (matrix[x][y] === "." || matrix[x][y] === "E")) {
        cx = x;
        cy = y;
        path.push([cx, cy]);
        visited.add(`${[cx,cy]}`);
        break;
      }
    }
    dist++;
  }

  const distanceMap = new Map();
  while (path.length > 0) {
    distanceMap.set(`${path.shift()}`, --dist);
  }
  return distanceMap;
}

function partOne(matrix, distancesFromEnd) {
  let sum = 0;
  for (const [coords, distance] of distancesFromEnd) {
    const [x, y] = coords.split(",").map(Number);
    for (const d of clipDirs) {
      const clips = d.map(([dx, dy]) => ([matrix[x + dx]?.[y + dy], x + dx, y + dy]));
      if (clips[0][0] == "#" && (clips[1][0] == "." || clips[1][0] == "E")) {
        const clipDist = distancesFromEnd.get(`${[clips[1][1],clips[1][2]]}`);
        if (clipDist != null && distance - clipDist - 2 >= 100) {
          sum++;
        }
      }
    }
  }
  return sum;
}

function partTwo(matrix) {
  let sum = 0;
  for (const [coords, distance] of distancesFromEnd) {
    const [x, y] = coords.split(",").map(Number);
    for (const d of clipDirs) {
      const clips = d.map(([dx, dy]) => ([matrix[x + dx]?.[y + dy], x + dx, y + dy]));
      if (clips[0][0] == "#" && (clips[1][0] == "." || clips[1][0] == "E")) {
        const clipDist = distancesFromEnd.get(`${[clips[1][1],clips[1][2]]}`);
        if (clipDist != null && distance - clipDist - 2 >= 100) {
          sum++;
        }
      }
    }
  }
  return sum;
}

function soln(rawInput) {
  const input = rawInput.split("\n").map(ln => ln.split(""));
  const distancesFromEnd = getDistances(input);
  fmtAnsWithRuntime(
    () => partOne(input, distancesFromEnd),
    () => partTwo(input, distancesFromEnd)
  );
}

module.exports = { soln };
