const {
  fmtAnsWithRuntime,
  getAllNeighborsWithCoordinates,
} = require("../util.js");

function findPlot([x, y], grid, plot, perimeter, corners) {
  const crop = grid[x][y];

  plot.add(`${[x, y]}`);

  const edges = Object.entries(getAllNeighborsWithCoordinates([x, y], grid));

  const matchingEdges = edges.filter(([_, e]) => e.value === crop);

  const { n, s, w, e, nw, sw, ne, se } = Object.fromEntries(matchingEdges);

  const nonMatchingEdges = edges.filter(
    ([_, e]) => e.value == null || e.value !== crop
  );

  for (const [d, e] of nonMatchingEdges) {
    if (["n", "s", "e", "w"].includes(d)) {
      perimeter.add(`${[x, y]}|${e.coords}`);
    }
  }

  const md = new Set(matchingEdges.map((it) => it[0]));

  for (const [d, e] of matchingEdges) {
    if (["n", "s", "e", "w"].includes(d) && !plot.has(`${e.coords}`)) {
      const f = [
        (!(w || n) || (w && n && !nw)),
        (!(w || s) || (w && s && !sw)),
        (!(e || n) || (e && n && !ne)),
        (!(e || s) || (e && s && !se)),
      ].filter(Boolean).length;
      corners.push(f);
      findPlot(e.coords, grid, plot, perimeter, corners);
    }
  }
}

function findUniqueLetters(grid) {
  const uniqueLetters = {};
  for (let x = 0; x < grid.length; x++) {
    for (let y = 0; y < grid[x].length; y++) {
      if (uniqueLetters[grid[x][y]] == null) {
        uniqueLetters[grid[x][y]] = [[x, y]];
      } else {
        uniqueLetters[grid[x][y]].push([x, y]);
      }
    }
  }
  return uniqueLetters;
}

function partOne(grid) {
  const uniqueLetters = findUniqueLetters(grid);
  let letters;
  let plots = [];
  while ((letters = Object.keys(uniqueLetters)).length > 0) {
    while (uniqueLetters[letters[0]].length > 0) {
      const plot = new Set();
      const perimeter = new Set();
      plots.push([plot, perimeter]);
      findPlot(uniqueLetters[letters[0]][0], grid, plot, perimeter, []);
      uniqueLetters[letters[0]] = uniqueLetters[letters[0]].filter(
        (letterCoord) => !plot.has(`${letterCoord}`)
      );
    }
    delete uniqueLetters[letters[0]];
  }
  return plots.reduce((total, [plot, perimeter]) => {
    return total + plot.size * perimeter.size;
  }, 0);
}

function partTwo(grid) {
  const uniqueLetters = findUniqueLetters(grid);
  let letters;
  let plots = [];
  while ((letters = Object.keys(uniqueLetters)).length > 0) {
    while (uniqueLetters[letters[0]].length > 0) {
      const plot = new Set();
      const corners = [];
      plots.push([plot, corners]);
      findPlot(uniqueLetters[letters[0]][0], grid, plot, new Set(), corners);
      uniqueLetters[letters[0]] = uniqueLetters[letters[0]].filter(
        (letterCoord) => !plot.has(`${letterCoord}`)
      );
    }
    delete uniqueLetters[letters[0]];
  }

  return plots.reduce((total, [plot, corners]) => {
    const c = corners.reduce((acc, c) => acc + c, 0);
    return total + plot.size * c;
  }, 0);
}

function soln(rawInput) {
  const input = rawInput.split("\n").map((ln) => ln.split(""));
  fmtAnsWithRuntime(
    () => partOne(input),
    () => partTwo(input)
  );
}

module.exports = { soln };
