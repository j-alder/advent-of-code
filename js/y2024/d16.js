const {
  fmtAnsWithRuntime,
  coordsOf,
  getNeighborsWithCoordinates,
  allCoordsOf,
  getAllNeighborsWithCoordinates,
} = require("../util.js");

const dirs = [
  [0, 1], // e
  [1, 0], // s
  [0, -1], // w
  [-1, 0] // n
]

const fmtKey = ([x, y], dir) => `${x},${y},${dir}`;

function getLowestScore(matrix, [sx, sy], [ex, ey]) {
  let score = 0;

  const queue = [[sx, sy, 2, 0]]; // queue of unvisited nodes
  const visited = new Set();

  while (queue.length) {
    queue.sort((a, b) => a[3] - b[3]); // sort by closest (smallest score)

    const [x, y, dir, score] = queue.shift(); // get the closest node
    const key = fmtKey([x, y], dir); // unique identifier with node and direction

    if (x === ex && y === ey) return score; // if the node is the end node, return the score -- exits early!
    if (visited.has(key)) continue; // if already visited, skip

    visited.add(key); // set visited

    const [fx, fy] = [x + dirs[dir][0], y + dirs[dir][1]]; // forward direction
    const left = (dir + 3) % 4;
    const right = (dir + 1) % 4;

    if (matrix[fx]?.[fy] !== "#") {
      queue.push([nx, ny, dir, score + 1]); // push forward direction into queue
    }

    // push all left/right dirs 
    if (matrix[x + dirs[right][0]][y + dirs[right]] !== "#") {
      queue.push([x, y, right, score + 1000]);
    }
    if (matrix[x + dirs[left][0]][y + dirs[left]] !== "#") {
      queue.push([x, y, left, score + 1000]);
    }
  }

  return score;
};

function partOne(matrix) {
  // 104504 too high
  // 103504 too high
  const start = coordsOf("S", matrix);
  const end = coordsOf("E", matrix);
  return getScore(matrix, start, end);
}

function partTwo(matrix) {}

function soln(rawInput) {
  const matrix = rawInput.split("\n").map((ln) => ln.split(""));
  fmtAnsWithRuntime(
    () => partOne(matrix),
    () => partTwo(matrix)
  );
}

module.exports = { soln };

/*
--- Day 16: Reindeer Maze ---
It's time again for the Reindeer Olympics! This year, the big event is the 
Reindeer Maze, where the Reindeer compete for the lowest score.

You and The Historians arrive to search for the Chief right as the event is about 
to start. It wouldn't hurt to watch a little, right?

The Reindeer start on the Start Tile (marked S) facing East and need to reach the 
End Tile (marked E). They can move forward one tile at a time (increasing their 
score by 1 point), but never into a wall (#). They can also rotate clockwise or 
counterclockwise 90 degrees at a time (increasing their score by 1000 points).

To figure out the best place to sit, you start by grabbing a map (your puzzle 
input) from a nearby kiosk. For example:

###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
There are many paths through this maze, but taking any of the best paths would 
incur a score of only 7036. This can be achieved by taking a total of 36 steps 
forward and turning 90 degrees a total of 7 times:


###############
#.......#....E#
#.#.###.#.###^#
#.....#.#...#^#
#.###.#####.#^#
#.#.#.......#^#
#.#.#####.###^#
#..>>>>>>>>v#^#
###^#.#####v#^#
#>>^#.....#v#^#
#^#.#.###.#v#^#
#^....#...#v#^#
#^###.#.#.#v#^#
#S..#.....#>>^#
###############
Here's a second example:

#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
In this maze, the best paths cost 11048 points; following one such path would 
look like this:

#################
#...#...#...#..E#
#.#.#.#.#.#.#.#^#
#.#.#.#...#...#^#
#.#.#.#.###.#.#^#
#>>v#.#.#.....#^#
#^#v#.#.#.#####^#
#^#v..#.#.#>>>>^#
#^#v#####.#^###.#
#^#v#..>>>>^#...#
#^#v###^#####.###
#^#v#>>^#.....#.#
#^#v#^#####.###.#
#^#v#^........#.#
#^#v#^#########.#
#S#>>^..........#
#################
Note that the path shown above includes one 90 degree turn as the very first 
move, rotating the Reindeer from facing East to facing North.

Analyze your map carefully. What is the lowest score a Reindeer could possibly 
get?
*/
