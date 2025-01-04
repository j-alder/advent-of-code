const { fmtAnsWithRuntime } = require('../util.js');

function makeExpand(starMap, n) {
  const galaxies = [];
  starMap.forEach((row, x) => {
    row.forEach((chr, y) => {
      if (chr === '#') {
        galaxies.push([x, y]);
      }
    });
  });
  const expandedGalaxies = Array.from(galaxies).map(el => Array.from(el));
  starMap.forEach((row, x) => {
    if (row.every(c => c === '.')) {
      for (let i = 0; i < galaxies.length; i++) {
        if (galaxies[i][0] > x) {
          expandedGalaxies[i][0] += n;
        }
      }
    }
  });
  starMap[0].forEach((_, y) => {
    let needsCol = true;
    for (let i = 0; i < starMap.length; i++) {
      if (starMap[i][y] !== '.') {
        needsCol = false;
        break;
      }
    }
    if (needsCol) {
      for (let i = 0; i < galaxies.length; i++) {
        if (galaxies[i][1] > y) {
          expandedGalaxies[i][1] += n;
        }
      }
    }
  });
  return expandedGalaxies;
}

function solution(input, expansionRate) {
  const galaxies = makeExpand(input, expansionRate > 1 ? expansionRate - 1 : expansionRate);
  const galaxyMap = {};
  galaxies.forEach((galaxy, i) => {
    const unMappedNeighbors = [];
    galaxies.forEach((galaxy2, j) => {
      if (i !== j && !galaxyMap[`${galaxy2[0]},${galaxy2[1]}`]) {
        unMappedNeighbors.push(galaxy2);
      }
    });
    galaxyMap[`${galaxy.toString().trim(/\[\]/)}`] = unMappedNeighbors;
  });
  let totalDistances = 0;
  Object.entries(galaxyMap).forEach(([coord, neighbors]) => {
    const [x, y] = coord.split(',').map(Number);
    neighbors.forEach(([x2, y2]) => {
      totalDistances += Math.abs(x - x2) + Math.abs(y - y2);
    });
  });
  return totalDistances;
}

function soln(rawInput) {
  const input = rawInput.split('\n').map(ln => ln.split(''));
  fmtAnsWithRuntime(
    () => solution(input, 1), 
    () => solution(input, 1000000));
}

module.exports = { soln };
