const { fmtAnsWithRuntime, coordsOf } = require("../util.js");

const gridValue = (grid, x, y) => grid[x]?.[y];

function move([dir, pos], grid) {
  switch (dir) {
    case "N":
      if (gridValue(grid, pos[0] - 1, pos[1]) == "#") {
        return ["E", [pos[0], pos[1] + 1]];
      }
      return [dir, [pos[0] - 1, pos[1]]];
    case "S":
      if (gridValue(grid, pos[0] + 1, pos[1]) == "#") {
        return ["W", [pos[0], pos[1] - 1]];
      }
      return [dir, [pos[0] + 1, pos[1]]];
    case "E":
      if (gridValue(grid, pos[0], pos[1] + 1) == "#") {
        return ["S", [pos[0] + 1, pos[1]]];
      }
      return [dir, [pos[0], pos[1] + 1]];
    case "W":
      if (gridValue(grid, pos[0], pos[1] - 1) == "#") {
        return ["N", [pos[0] - 1, pos[1]]];
      }
      return [dir, [pos[0], pos[1] - 1]];
    default:
      return [dir, pos];
  }
}

const visitedStrFrom = ([dir, coords]) => `${dir}|${coords[0]},${coords[1]}`;

function traverseFrom([dir, coords], grid) {
  let curr = [dir, Array.from(coords)];
  const height = grid.length;
  const width = grid[0].length;
  const path = [];
  while (curr[1][0] < height && curr[1][1] < width) {
    path.push(curr);
    curr = move(curr, grid);
  }
  return path;
}

function partOne(grid) {
  let curr = ["N", coordsOf("^", grid)];
  const path = traverseFrom(curr, grid);
  const visited = new Set();
  path.forEach(([_, coords]) => visited.add(`${coords[0]},${coords[1]}`));
  return visited.size;
}

function isLoop([dir, coords], grid) {
  let curr = [dir, Array.from(coords)];
  const visited = new Set();
  while (gridValue(grid, curr[1][0], curr[1][1]) != null) {
    if (visited.has(visitedStrFrom(curr))) {
      return true;
    }
    visited.add(visitedStrFrom(curr));
    curr = move(curr, grid);
  }
  return false;
}

function getObstacleLocation([currDir, currCoords]) {
  switch (currDir) {
    case "N":
      return [currCoords[0] - 1, currCoords[1]];
    case "S":
      return [currCoords[0] + 1, currCoords[1]];
    case "E":
      return [currCoords[0], currCoords[1] - 1];
    case "W":
      return [currCoords[0], currCoords[1] + 1];
  }
}

function createNewGridWithObstacleAt([x, y], grid) {
  const newGrid = [...grid.map(it => it.map(it_ => it_))];
  newGrid[x][y] = "#";
  return newGrid;
}

function findLoopOpportunities(path, grid, count) {
  if (path.length < 2) {
    return 0;
  }
  // if the last position in the path is not a turn, insert an O
  if (path[path.length-1][0] !== path[path.length-2][0]) {
    return 0 + findLoopOpportunities(path.slice(0, path.length-1), grid, count);
  }
  const mutatedGrid = createNewGridWithObstacleAt(getObstacleLocation(path[path.length-1]), grid);
  if (isLoop(path[0], mutatedGrid)) {
    return 1 + findLoopOpportunities(path.slice(0, path.length-1), grid, count);
  } else {
    return 0 + findLoopOpportunities(path.slice(0, path.length-1), grid, count);
  }
}

function partTwo(grid) {
  let curr = ["N", coordsOf("^", grid)];
  const originalPath = traverseFrom(curr, grid);
  return findLoopOpportunities(originalPath.splice(0, originalPath.length - 1), grid, 0);
}

function soln(rawInput) {
  const input = rawInput.split("\n").map((ln) => ln.split(""));
  fmtAnsWithRuntime(
    () => partOne(input),
    () => partTwo(input)
  );
}

module.exports = { soln };

/*
--- Day 6: Guard Gallivant ---
The Historians use their fancy device again, this time to whisk you all away to the
North Pole prototype suit manufacturing lab... in the year 1518! It turns out that
having direct access to history is very convenient for a group of historians.

You still have to be careful of time paradoxes, and so it will be important to
avoid anyone from 1518 while The Historians search for the Chief. Unfortunately, a
single guard is patrolling this part of the lab.

Maybe you can work out where the guard will go ahead of time so that The
Historians can search safely?

You start by making a map (your puzzle input) of the situation. For example:

....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
The map shows the current position of the guard with ^ (to indicate the guard is
currently facing up from the perspective of the map). Any obstructions - crates,
desks, alchemical reactors, etc. - are shown as #.

Lab guards in 1518 follow a very strict patrol protocol which involves repeatedly
following these steps:

If there is something directly in front of you, turn right 90 degrees.
Otherwise, take a step forward.
Following the above protocol, the guard moves up several times until she reaches an
obstacle (in this case, a pile of failed suit prototypes):

....#.....
....^....#
..........
..#.......
.......#..
..........
.#........
........#.
#.........
......#...
Because there is now an obstacle in front of the guard, she turns right before
continuing straight in her new facing direction:

....#.....
........>#
..........
..#.......
.......#..
..........
.#........
........#.
#.........
......#...
Reaching another obstacle (a spool of several very long polymers), she turns right
again and continues downward:

....#.....
.........#
..........
..#.......
.......#..
..........
.#......v.
........#.
#.........
......#...
This process continues for a while, but the guard eventually leaves the mapped area
(after walking past a tank of universal solvent):

....#.....
.........#
..........
..#.......
.......#..
..........
.#........
........#.
#.........
......#v..
By predicting the guard's route, you can determine which specific positions in the
lab will be in the patrol path. Including the guard's starting position, the
positions visited by the guard before leaving the area are marked with an X:

....#.....
....XXXXX#
....X...X.
..#.X...X.
..XXXXX#X.
..X.X.X.X.
.#XXXXXXX.
.XXXXXXX#.
#XXXXXXX..
......#X..
In this example, the guard will visit 41 distinct positions on your map.

Predict the path of the guard. How many distinct positions will the guard visit
before leaving the mapped area?

--- Part Two ---
While The Historians begin working around the guard's patrol route, you borrow their
fancy device and step outside the lab. From the safety of a supply closet, you time
travel through the last few months and record the nightly status of the lab's guard
post on the walls of the closet.

Returning after what seems like only a few seconds to The Historians, they explain
that the guard's patrol area is simply too large for them to safely search the lab
without getting caught.

Fortunately, they are pretty sure that adding a single new obstruction won't cause
a time paradox. They'd like to place the new obstruction in such a way that the
guard will get stuck in a loop, making the rest of the lab safe to search.

To have the lowest chance of creating a time paradox, The Historians would like to
know all of the possible positions for such an obstruction. The new obstruction
can't be placed at the guard's starting position - the guard is there right now
and would notice.

In the above example, there are only 6 different positions where a new obstruction
would cause the guard to get stuck in a loop. The diagrams of these six situations
use O to mark the new obstruction, | to show a position where the guard moves
up/down, - to show a position where the guard moves left/right, and + to show a
position where the guard moves both up/down and left/right.

Option one, put a printing press next to the guard's starting position:

....#.....
....+---+#
....|...|.
..#.|...|.
....|..#|.
....|...|.
.#.O^---+.
........#.
#.........
......#...
Option two, put a stack of failed suit prototypes in the bottom right quadrant of
the mapped area:


....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
......O.#.
#.........
......#...
Option three, put a crate of chimney-squeeze prototype fabric next to the standing
desk in the bottom right quadrant:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
.+----+O#.
#+----+...
......#...
Option four, put an alchemical retroencabulator near the bottom left corner:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
..|...|.#.
#O+---+...
......#...
Option five, put the alchemical retroencabulator a bit to the right instead:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
....|.|.#.
#..O+-+...
......#...
Option six, put a tank of sovereign glue right next to the tank of universal
solvent:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
.+----++#.
#+----++..
......#O..
It doesn't really matter what you choose to use as an obstacle so long as you and
The Historians can put it into position without the guard noticing. The important
thing is having enough options that you can find one that minimizes time paradoxes,
and in this example, there are 6 different positions you could choose.

You need to get the guard stuck in a loop by adding a single new obstruction. How
many different positions could you choose for this obstruction?
*/
