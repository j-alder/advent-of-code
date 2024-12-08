const { fmtAnsWithRuntime } = require("../util.js");

const antiNodeLocs = (a, b) => [
  [a[0] + (a[0] - b[0]), a[1] + (a[1] - b[1])],
  [b[0] + (b[0] - a[0]), b[1] + (b[1] - a[1])],
];

const withinBounds = ([x, y], h, w) => x >= 0 && x < h && y >= 0 && y < w;

function getLocationMap(input) {
  const matches = input.flatMap((ln) => ln.matchAll(/[0-9a-zA-Z]/g));
  const matchTriples = matches.flatMap((matchIterator, i) =>
    [...matchIterator].map((match) => [match[0], i, match.index])
  );
  return matchTriples.reduce((acc, ent) => {
    if (acc[ent[0]] == null) acc[ent[0]] = [[ent[1], ent[2]]];
    else acc[ent[0]].push([ent[1], ent[2]]);
    return acc;
  }, {});
}

function partOne(input) {
  const h = input.length;
  const w = input[0].length;
  const locMap = getLocationMap(input);
  return Object.values(locMap).reduce((acc, coords) => {
    for (let i = 0; i < coords.length; i++) {
      for (let j = 0; j < coords.length; j++) {
        if (j != i) {
          const potentialNodes = antiNodeLocs(coords[i], coords[j]);
          if (withinBounds(potentialNodes[0], h, w)) {
            acc.add(potentialNodes[0].join(","));
          }
          if (withinBounds(potentialNodes[1], h, w)) {
            acc.add(potentialNodes[1].join(","));
          }
        }
      }
    }
    return acc;
  }, new Set()).size;
}

const antiNodeLoc = (a, b) => [
  [a[0] + (a[0] - b[0]), a[1] + (a[1] - b[1])],
  [b[0] + (b[0] - a[0]), b[1] + (b[1] - a[1])],
];

function allAntiNodes(a, b, h, w) {
  const result = new Set();
  let [a_, b_] = antiNodeLocs(a, b);
  result.add(`${a[0]},${a[1]}`);
  result.add(`${b[0]},${b[1]}`);
  let tmpA = [...a];
  let tmpB = [...b];
  let changed = true;
  while (changed) {
    changed = false;
    if (withinBounds(a_, h, w)) {
      result.add(`${a_[0]},${a_[1]}`);
      changed = true;
      const nodes = antiNodeLocs(a_, tmpA);
      tmpA = [...a_];
      a_ = nodes[0];
    }
    if (withinBounds(b_, h, w)) {
      result.add(`${b_[0]},${b_[1]}`);
      changed = true;
      const nodes = antiNodeLocs(tmpB, b_);
      tmpB = [...b_];
      b_ = nodes[1];
    }
  }
  return result;
}

function partTwo(input) {
  const h = input.length;
  const w = input[0].length;
  const locMap = getLocationMap(input);
  return Object.values(locMap).reduce((acc, coords) => {
    for (let i = 0; i < coords.length; i++) {
      for (let j = 0; j < coords.length; j++) {
        if (j != i) {
          const antiNodes = allAntiNodes(coords[i], coords[j], h, w);
          antiNodes.forEach((an) => acc.add(an));
        }
      }
    }
    return acc;
  }, new Set()).size;
}

function soln(rawInput) {
  const input = rawInput.split("\n");
  fmtAnsWithRuntime(
    () => partOne(input),
    () => partTwo(input)
  );
}

module.exports = { soln };

/*
--- Day 8: Resonant Collinearity ---
You find yourselves on the roof of a top-secret Easter Bunny installation.

While The Historians do their thing, you take a look at the familiar huge antenna. 
Much to your surprise, it seems to have been reconfigured to emit a signal that 
makes people 0.1% more likely to buy Easter Bunny brand Imitation Mediocre 
Chocolate as a Christmas gift! Unthinkable!

Scanning across the city, you find that there are actually many such antennas. 
Each antenna is tuned to a specific frequency indicated by a single lowercase 
letter, uppercase letter, or digit. You create a map (your puzzle input) of these 
antennas. For example:

............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............

The signal only applies its nefarious effect at specific antinodes based on the 
resonant frequencies of the antennas. In particular, an antinode occurs at any 
point that is perfectly in line with two antennas of the same frequency - but only 
when one of the antennas is twice as far away as the other. This means that for 
any pair of antennas with the same frequency, there are two antinodes, one on 
either side of them.

So, for these two antennas with frequency a, they create the two antinodes marked 
with #:

..........
...#......
..........
....a.....
..........
.....a....
..........
......#...
..........
..........

Adding a third antenna with the same frequency creates several more antinodes. It 
would ideally add four antinodes, but two are off the right side of the map, so 
instead it adds only two:

..........
...#......
#.........
....a.....
........a.
.....a....
..#.......
......#...
..........
..........

Antennas with different frequencies don't create antinodes; A and a count as 
different frequencies. However, antinodes can occur at locations that contain 
antennas. In this diagram, the lone antenna with frequency capital A creates no 
antinodes but has a lowercase-a-frequency antinode at its location:

..........
...#......
#.........
....a.....
........a.
.....a....
..#.......
......A...
..........
..........

The first example has antennas with two different frequencies, so the antinodes 
they create look like this, plus an antinode overlapping the topmost A-frequency 
antenna:

......#....#
...#....0...
....#0....#.
..#....0....
....0....#..
.#....A.....
...#........
#......#....
........A...
.........A..
..........#.
..........#.

Because the topmost A-frequency antenna overlaps with a 0-frequency antinode, 
there are 14 total unique locations that contain an antinode within the bounds of 
the map.

Calculate the impact of the signal. How many unique locations within the bounds of 
the map contain an antinode?

--- Part Two ---
Watching over your shoulder as you work, one of The Historians asks if you took 
the effects of resonant harmonics into your calculations.

Whoops!

After updating your model, it turns out that an antinode occurs at any grid 
position exactly in line with at least two antennas of the same frequency, 
regardless of distance. This means that some of the new antinodes will occur at 
the position of each antenna (unless that antenna is the only one of its 
frequency).

So, these three T-frequency antennas now create many antinodes:

T....#....
...T......
.T....#...
.........#
..#.......
..........
...#......
..........
....#.....
..........
In fact, the three T-frequency antennas are all exactly in line with two 
antennas, so they are all also antinodes! This brings the total number of 
antinodes in the above example to 9.

The original example now has 34 antinodes, including the antinodes that appear on 
every antenna:

##....#....#
.#.#....0...
..#.#0....#.
..##...0....
....0....#..
.#...#A....#
...#..#.....
#....#.#....
..#.....A...
....#....A..
.#........#.
...#......##
Calculate the impact of the signal using this updated model. How many unique 
locations within the bounds of the map contain an antinode?
*/
