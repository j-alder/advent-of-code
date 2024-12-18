const { fmtAnsWithRuntime, coordsOf } = require("../util.js");

const dirs = [
  [0, 1], // e
  [1, 0], // s
  [0, -1], // w
  [-1, 0], // n
];

const arrow = {
  0: ">",
  1: "v",
  2: "<",
  3: "^",
};

function getLowestScore(matrix, [sx, sy], [ex, ey]) {
  const queue = [[sx, sy, 0, 0]];
  const visited = new Set();

  while (queue.length) {
    queue.sort((a, b) => a[3] - b[3]);

    const [x, y, dir, score] = queue.shift();
    const key = `${x},${y},${dir}`;

    if (x === ex && y === ey) {
      return score;
    }

    if (visited.has(key)) {
      continue;
    }

    visited.add(key);

    const [fx, fy] = [x + dirs[dir][0], y + dirs[dir][1]]; // forward direction

    if (matrix[fx]?.[fy] !== "#") {
      queue.push([fx, fy, dir, score + 1]); // push forward direction into queue
    }

    queue.push([x, y, (dir + 1) % 4, score + 1000]);
    queue.push([x, y, (dir + 3) % 4, score + 1000]);
  }
  return -1;
}

function partOne(matrix) {
  const start = coordsOf("S", matrix);
  const end = coordsOf("E", matrix);
  return getLowestScore(matrix, start, end);
}

function getAllPathsWithScore(matrix, [sx, sy], [ex, ey], lowestScore) {
  const queue = [[sx, sy, 0, 0]];
  const visited = new Set();

  while (queue.length) {
    queue.sort((a, b) => a[3] - b[3]);

    const [x, y, dir, score] = queue.shift();
    const key = `${x},${y},${dir}`;

    if (x === ex && y === ey) {
      return score;
    }

    if (visited.has(key) || score > lowestScore) {
      continue;
    }

    visited.add(key);

    const [fx, fy] = [x + dirs[dir][0], y + dirs[dir][1]]; // forward direction

    if (matrix[fx]?.[fy] !== "#") {
      queue.push([fx, fy, dir, score + 1]); // push forward direction into queue
    }

    queue.push([x, y, (dir + 1) % 4, score + 1000]);
    queue.push([x, y, (dir + 3) % 4, score + 1000]);
  }
  return -1;

}

function partTwo(matrix) {
  const start = coordsOf("S", matrix);
  const end = coordsOf("E", matrix);
  const lowestScore = getLowestScore(matrix, start, end);

}

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

--- Part Two ---
Now that you know what the best paths look like, you can figure out the best spot 
to sit.

Every non-wall tile (S, ., or E) is equipped with places to sit along the edges 
of the tile. While determining which of these tiles would be the best spot to sit 
depends on a whole bunch of factors (how comfortable the seats are, how far away 
the bathrooms are, whether there's a pillar blocking your view, etc.), the most 
important factor is whether the tile is on one of the best paths through the 
maze. If you sit somewhere else, you'd miss all the action!

So, you'll need to determine which tiles are part of any best path through the 
maze, including the S and E tiles.

In the first example, there are 45 tiles (marked O) that are part of at least 
one of the various best paths through the maze:

###############
#.......#....O#
#.#.###.#.###O#
#.....#.#...#O#
#.###.#####.#O#
#.#.#.......#O#
#.#.#####.###O#
#..OOOOOOOOO#O#
###O#O#####O#O#
#OOO#O....#O#O#
#O#O#O###.#O#O#
#OOOOO#...#O#O#
#O###.#.#.#O#O#
#O..#.....#OOO#
###############

In the second example, there are 64 tiles that are part of at least one of the 
best paths:

#################
#...#...#...#..O#
#.#.#.#.#.#.#.#O#
#.#.#.#...#...#O#
#.#.#.#.###.#.#O#
#OOO#.#.#.....#O#
#O#O#.#.#.#####O#
#O#O..#.#.#OOOOO#
#O#O#####.#O###O#
#O#O#..OOOOO#OOO#
#O#O###O#####O###
#O#O#OOO#..OOO#.#
#O#O#O#####O###.#
#O#O#OOOOOOO..#.#
#O#O#O#########.#
#O#OOO..........#
#################

Analyze your map further. How many tiles are part of at least one of the best 
paths through the maze?
*/
