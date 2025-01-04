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
