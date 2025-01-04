const { 
  fmtAnsWithRuntime, 
  isPalindrome, 
  rotateMatrixAntiClockwise 
} = require('../util.js');

function getColsLeft(matrix) {
  const plen = matrix[0].length;
  for (let i = 0; i < matrix[0].length - 1; i++) {
    if (matrix.every(ln => isPalindrome(ln.substring(i)))) {
      return Math.ceil(i + (plen - i) / 2);
    }
  }
  for (let i = matrix[0].length - 1; i > 1; i--) {
    if (matrix.every(ln => isPalindrome(ln.substring(0, i)))) {
      return Math.floor(i / 2);
    }
  }
  return 0;
}

function rotateStrMatrix(matrix) {
  return rotateMatrixAntiClockwise(matrix.map(ln => ln.split(''))).map(ln => ln.join(''));
}

function partOne(input) {
  let cnt = 0;
  input.map(pattern => pattern.split('\n'))
    .forEach(x => {
      cnt += getColsLeft(x);
      cnt += 100 * getColsLeft(rotateStrMatrix(x));
    });
  return cnt;
}

function partTwo(input) {
}

function soln(rawInput) {
  const input = rawInput.split('\n\n');
  fmtAnsWithRuntime(() => partOne(input), () => partTwo(input));
}

module.exports = { soln };
