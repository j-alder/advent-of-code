/**
 * Find the coordinates of a value in a 2d array. Returns undefined
 * if not found.
 * 
 * @param {any} val value to check for
 * @param {any[][]} matrix 2 dimensional array to check
 * @returns {number[] | undefined} [x, y] coordinates of value
 */
function coordsOf(val, matrix) {
  let x = 0;
  while (x < matrix.length) {
    const y = matrix[x].indexOf(val);
    if (y > -1) return [x, y];
    x++;
  }
}

/**
 * Execute a function and record its runtime.
 * 
 * @param {*} f The function to execute
 * @returns {Array<string, number>} A tuple with the function result and the runtime
 */
function withRuntime(f) {
  const start = performance.now();
  return [f(), performance.now() - start];
}

/**
 * Format the output of two callbacks with their runtimes
 * 
 * @param {function()} partOne 
 * @param {function()} partTwo 
 */
function fmtAnsWithRuntime(partOne, partTwo) {
  const [ansOne, rtOne] = withRuntime(partOne)
  console.log(`
  part one: ${ansOne ?? 'incomplete'}${ansOne ? `\n  runtime (ms): ${rtOne.toFixed(4)}` : ''}`);
  const [ansTwo, rtTwo] = withRuntime(partTwo);
  console.log(`
  part two: ${ansTwo ?? 'incomplete'}${ansTwo ? `\n  runtime (ms): ${rtTwo.toFixed(4)}` : ''}`);
}

/**
 * Return all neighbors of a given coordinate in a matrix, along with their coordinates within
 * the matrix.
 * 
 * @param {Number} x index of outer array
 * @param {Number} y index of inner array
 * @param {any[][]} matrix 2d array
 * @returns {Object.<string, [any, number, number] | undefined>} An object whose keys are cardinal 
 * directions surrounding element at x, y and values are triples containing the element at that 
 * direction, its x value, and its y value
 */
const getNeighborsWithCoordinates = (x, y, matrix) =>
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

const defaultDirections = {
  nw: [-1, -1],
  n: [-1, 0],
  ne: [-1, 1],
  e: [0, -1],
  w: [0, 1],
  sw: [1, -1],
  s: [1, 0],
  se: [1, 1]
};

/**
 * Get all neighbors of a specific element in a matrix at coordinates x and y.
 * 
 * @param {Number} x index of outer array
 * @param {Number} y index of inner array
 * @param {Object<string, [Number, Number]> | undefined} directions A map of direction names to how
 * far away they are on the coordinate axis
*/
const getNeighbors = (x, y, matrix, directions = defaultDirections) =>
  Object.fromEntries(
    Object.entries(directions).map(([k, dir]) => (
      [k, matrix[x + dir[0]]?.[y + dir[1]]]
    )));


const isPalindrome = (str) =>
  str === str.split('').reverse().join('');

/**
 * Rotate a 2d array 90 degrees anti-clockwise
 * 
 * @param {any[][]} matrix 
 * @returns {any[][]}
 */
function rotateMatrixAntiClockwise(matrix) {
  const len = matrix[0].length;
  const out = [];
  for (let i = 0; i < len; i++) { // 0 - 2
    out[i] = [];
    for (let j = 0; j < matrix.length; j++) { // 0 - 1
      out[i][j] = matrix[j][len - i - 1];
    }
  }
  return out;
}

/**
 * Rotate a 2d array 90 degrees clockwise
 * 
 * @param {any[][]} matrix 
 * @returns {any[][]}
 */
function rotateMatrixClockwise(matrix) {
  const len = matrix[0].length;
  const out = [];
  for (let i = 0; i < len; i++) {
    out[i] = [];
    for (let j = 0; j < matrix.length; j++) {
      out[i][j] = matrix[matrix.length - j - 1][i];
    }
  }
  return out;
}

const sum = (arr) => arr.reduce((s, n) => s + n, 0);

/** 
 * Greatest common denominator of a and b
 * 
 * @param {number} a
 * @param {number} b
 */
function gcd(a, b) {
  while (b) {
    [a, b] = [b, a % b];
  }
  return a;
}

/** 
 * Least common multiple of a and b
 * 
 * @param {number} a
 * @param {number} b
 */
function lcm(a, b) {
  return (a * b) / gcd(a, b);
}

/**
 * A map of elements in an array to the number of times they 
 * occur within it.
 * 
 * @param {Array<number | string>} arr 
 */
function occurrances(arr) {
  return arr.reduce((acc, elem) => {
    if (acc[elem] != null) {
      acc[elem] = acc[elem] + 1
    } else {
      acc[elem] = 1
    }
    return acc;
  }, {})
}

/**
 * Determine whether a number is between two other numbers,
 * non-inclusive.
 * 
 * e.g. between(5, 0, 5) = false
 * e.g. between(1, 1, 3) = false
 * e.g. between(1, 0, 3) = true
 * 
 * @param {number} n The number to check
 * @param {number} a Non-inclusive start of range
 * @param {number} b Non-inclusive end of range
 */
const between = (n, a, b) => n > a && n < b;

module.exports = {
  coordsOf,
  fmtAnsWithRuntime,
  getNeighborsWithCoordinates,
  getNeighbors,
  isPalindrome,
  rotateMatrixAntiClockwise,
  rotateMatrixClockwise,
  sum,
  gcd,
  lcm,
  occurrances,
  between
};
