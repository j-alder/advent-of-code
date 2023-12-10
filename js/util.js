function coordsOf(val, matrix) {
  let x = 0;
  while (x < matrix.length) {
    const y = matrix[x].indexOf(val);
    if (y > -1) return [x, y];
    x++;
  }
}

function withRuntime(cb) {
  const start = performance.now();
  return [cb(), performance.now() - start];
}

function fmtAnsWithRuntime(cb1, cb2) {
  const [ansOne, rtOne] = withRuntime(cb1)
  console.log(`
  part one: ${ansOne ?? 'incomplete'}${ansOne ? `\n  runtime (ms): ${rtOne.toFixed(4)}` : ''}`);
  const [ansTwo, rtTwo] = withRuntime(cb2);
  console.log(`
  part two: ${ansTwo ?? 'incomplete'}${ansTwo ? `\n  runtime (ms): ${rtTwo.toFixed(4)}` : ''}`);
}

/**
 * Return all neighbors of a given coordinate in a matrix
 * @param {*} x index of outer array
 * @param {*} y index of inner array
 * @param {*} matrix 2d array
 * @returns An object with each neighbor as a key and its coordinates as a value
 */
const getNeighbors = (x, y, matrix) =>
  ({
    nw: matrix[x - 1] && [matrix[x - 1][y - 1], x - 1, y - 1],
    n: matrix[x - 1] && [matrix[x - 1][y], x - 1, y],
    ne: matrix[x - 1] && [matrix[x - 1][y + 1], x - 1, y + 1],
    w: matrix[x] && [matrix[x][y - 1], x, y - 1],
    e: matrix[x] && [matrix[x][y + 1], x, y + 1],
    sw: matrix[x + 1] && [matrix[x + 1][y - 1], x + 1, y - 1],
    s: matrix[x + 1] && [matrix[x + 1][y], x + 1, y],
    se: matrix[x + 1] && [matrix[x + 1][y + 1], x + 1, y + 1],
  });

function readInput(year, day, test) {
  const fs = require('node:fs');
  const file = fs.readFileSync(`../input/y${year}/d${day < 10 ? `0${day}` : day}${test ? '_test' : ''}.txt`, 'utf8');
  return file;
}

const sum = (arr) => arr.reduce((s, n) => s + n, 0);

module.exports = {
  coordsOf,
  fmtAnsWithRuntime,
  getNeighbors,
  readInput,
  sum,
};

