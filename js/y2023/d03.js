const { fmtAnsWithRuntime } = require('../util.js');

const getSurrounding = (matrix, [a, b]) => 
  [
    [matrix[a-1][b-1], [a-1, b-1]],
    [matrix[a-1][b], [a-1, b]],
    [matrix[a-1][b+1], [a-1, b+1]],
    [matrix[a][b-1], [a, b-1]],
    [matrix[a][b+1], [a, b+1]],
    [matrix[a+1][b-1], [a+1, b-1]],
    [matrix[a+1][b], [a+1, b]],
    [matrix[a+1][b+1], [a+1, b+1]],
  ];

function partOne(input) {
  let total = 0;
  for (let a = 0; a < input.length; a++) {
    for (let b = 0; b < input[a].length; b++) {
      if (!/(\d{1}|\.)/.test(input[a][b])) {
        const adj = getSurrounding(input, [a, b]);
        for (let [_, [cx, cy]] of adj) {
          let number = '';
          if (/\d/.test(input[cx][cy])) {
            number += input[cx][cy];
            input[cx][cy] = '.';
            let left = cy - 1;
            while ( /\d/.test(input[cx][left])) {
              number = input[cx][left] + number;
              input[cx][left] = '.';
              left--;
            }
            let right = cy + 1;
            while (/\d/.test(input[cx][right])) {
              number = number + input[cx][right];
              input[cx][right] = '.';
              right++;
            }
            total += Number(number);
          }
        }
      }
    }
  }
  return total;
}

function partTwo(input) {
  let total = 0;
  for (let a = 0; a < input.length; a++) {
    for (let b = 0; b < input[a].length; b++) {
      if (/(\*)/.test(input[a][b])) {
        const adj = getSurrounding(input, [a, b]);
        const partNumbers = [];
        for (let [_, [cx, cy]] of adj) {
          let number = '';
          if (/\d/.test(input[cx][cy])) {
            number += input[cx][cy];
            input[cx][cy] = '.';
            let left = cy - 1;
            while ( /\d/.test(input[cx][left])) {
              number = input[cx][left] + number;
              input[cx][left] = '.';
              left--;
            }
            let right = cy + 1;
            while (/\d/.test(input[cx][right])) {
              number = number + input[cx][right];
              input[cx][right] = '.';
              right++;
            }
            partNumbers.push(Number(number));
          }
        }
        if (partNumbers.length === 2) total += partNumbers[0] * partNumbers[1];
      }
    }
  }
  return total;
}

function soln(rawInput) {
  const input1 = rawInput.split('\n').map(it => it.split(''));
  const input2 = rawInput.split('\n').map(it => it.split(''));
  fmtAnsWithRuntime(() => partOne(input1), () => partTwo(input2));
}

module.exports = { soln };
