const {
  fmtAnsWithRuntime,
  allCoordsOf,
  getNeighborsWithCoordinates,
} = require("../util.js");

function findUniquePositions([x, y], grid, uniquePositions) {
  if (grid[x]?.[y] == null) return;
  const neighbors = getNeighborsWithCoordinates([x, y], grid, {
    n: [-1, 0],
    e: [0, -1],
    w: [0, 1],
    s: [1, 0],
  });
  const currElev = grid[x][y];
  if (currElev == 9) {
    if (!uniquePositions.has(`${[x, y]}`)) uniquePositions.add(`${[x, y]}`);
    return;
  }
  Object.entries(neighbors)
    .filter(([_, info]) => info != null && info.value - currElev === 1)
    .forEach(([_, info]) =>
      findUniquePositions(info.coords, grid, uniquePositions)
    );
}

function rateTrailhead([x, y], grid) {
  if (grid[x]?.[y] == null) return;
  const neighbors = getNeighborsWithCoordinates([x, y], grid, {
    n: [-1, 0],
    e: [0, -1],
    w: [0, 1],
    s: [1, 0],
  });
  const currElev = grid[x][y];
  if (currElev == 9) {
    return 1;
  }
  return Object.entries(neighbors)
    .filter(([_, info]) => info != null && info.value - currElev === 1)
    .reduce((acc, [_, info]) => acc + rateTrailhead(info.coords, grid), 0);
}

function partOne(grid) {
  const trailheads = allCoordsOf(0, grid);
  let total = 0;
  for (th of trailheads) {
    const uniquePositions = new Set();
    findUniquePositions(th, grid, uniquePositions);
    total += uniquePositions.size;
  }
  return total;
}

function partTwo(grid) {
  const trailheads = allCoordsOf(0, grid);
  let total = 0;
  for (th of trailheads) {
    total += rateTrailhead(th, grid);
  }
  return total;
}

function soln(rawInput) {
  const input = rawInput.split("\n").map((ln) => ln.split("").map(Number));
  fmtAnsWithRuntime(
    () => partOne(input),
    () => partTwo(input)
  );
}

module.exports = { soln };
