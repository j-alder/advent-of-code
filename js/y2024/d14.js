const {
  fmtAnsWithRuntime,
  betweenInc,
  getAllNeighborsWithCoordinates,
  sum,
  rotateMatrixClockwise,
} = require("../util.js");
const fs = require("node:fs");

const xlen = 101;
const ylen = 103;

const move = ([py, px], [vy, vx]) => {
  return [
    (((py + vy) % ylen) + ylen) % ylen,
    (((px + vx) % xlen) + xlen) % xlen,
  ];
};

function partOne(robots) {
  for (let sec = 0; sec < 100; sec++) {
    for (const robot of robots) {
      robot.pos = move(robot.pos, robot.vel);
    }
  }

  const [q1, q2, q3, q4] = Object.values(robots).reduce(
    (acc, { pos: [y, x], _ }) => {
      if (betweenInc(x, 0, Math.floor(xlen / 2) - 1)) {
        if (betweenInc(y, 0, Math.floor(ylen / 2) - 1)) {
          acc[0]++;
        }
        if (betweenInc(y, Math.ceil(ylen / 2), ylen - 1)) {
          acc[1]++;
        }
      }
      if (betweenInc(x, Math.ceil(xlen / 2), xlen - 1)) {
        if (betweenInc(y, 0, Math.floor(ylen / 2) - 1)) {
          acc[2]++;
        }
        if (betweenInc(y, Math.ceil(ylen / 2), ylen - 1)) {
          acc[3]++;
        }
      }
      return acc;
    },
    [0, 0, 0, 0]
  );
  return q1 * q2 * q3 * q4;
}

/**
 *
 * @param {[number, number]} param0
 * @param {number[][]} image
 * @param {Set<string>} visited
 * @param {Set<string>} uniquePositions
 */
function fill([y, x], image, uniquePositions) {
  uniquePositions.delete(`${[y, x]}`);

  const edges = Object.values(getAllNeighborsWithCoordinates([y, x], image));

  const matchingEdges = edges.filter((edge) => (edge.value ?? 0) > 0);

  for (const edge of matchingEdges) {
    if (uniquePositions.has(`${[y, x]}`)) {
      fill(edge.coords, image, uniquePositions);
    }
  }
}

function variance(nums) {
  const mean = sum(nums) / nums.length;
  return nums.reduce((acc, n) => acc + Math.pow(n - mean, 2), 0) / nums.length;
}

function avgVariance(robots) {
  const allX = robots.map(({ pos }) => pos[1]);
  const varX = variance(allX);
  const allY = robots.map(({ pos }) => pos[0]);
  const varY = variance(allY);
  return (varX + varY) / 2;
}

function partTwo(robots) {
  const image = Array.from({ length: xlen }, () =>
    Array.from({ length: ylen }, () => 0)
  );
  robots.forEach(({ pos, _ }) => {
    image[pos[1]][pos[0]] = (image[pos[1]][pos[0]] ?? 0) + 1;
  });

  let lowestAvgVar = [Number.MAX_SAFE_INTEGER, -1];

  for (let sec = 1; sec < 20_000; sec++) {
    for (const robot of robots) {
      image[robot.pos[1]][robot.pos[0]]--;
      robot.pos = move(robot.pos, robot.vel);
      image[robot.pos[1]][robot.pos[0]]++;
    }

    const avgVar = avgVariance(robots);
    if (avgVar < lowestAvgVar[0]) {
      lowestAvgVar = [avgVar, sec];
    }

    if (avgVariance(robots) < 500) {
      fs.writeFile(
        `/Users/jonathon/Code/advent-of-code/js/y2024/d14out/d14-${sec}.txt`,
        rotateMatrixClockwise(image)
          .map((row) => row.map((it) => (it > 0 ? "#" : " ")).join(""))
          .join("\n"),
        (err) => {
          if (err) console.log(err, sec);
        }
      );
    }
  }
  return lowestAvgVar[1];
}

function soln(rawInput) {
  const input = rawInput.split("\n").map((ln) => {
    [pos, vel] = [...ln.matchAll(/-?\d+,-?\d+/g)].map((m) =>
      m[0].split(",").map(Number)
    );
    return { pos: [pos[1], pos[0]], vel: [vel[1], vel[0]] };
  });
  fmtAnsWithRuntime(
    () => partOne(input),
    () => partTwo(input)
  );
}

module.exports = { soln };
