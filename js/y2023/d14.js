const { fmtAnsWithRuntime } = require('../util.js');

const calcRowLoad = (multiplier, row) => 
  row.reduce((rocks, char) => rocks + (char === 'O' ? 1 : 0), 0) * multiplier;

const calcDishLoad = (dish) => 
  dish.reduce((load, row, i) => load + calcRowLoad(dish.length - i, row), 0);

function tiltNorth(dish) {
  for (let i = 0; i < dish[0].length; i++) {
    let curr = 0;
    for (let j = 0; j < dish.length; j++) {
      if (dish[j][i] === 'O') {
        if (j > curr) {
          dish[j][i] = '.';
          dish[curr][i] = 'O';
          curr = curr + 1;
        } else {
          curr = j + 1;
        }
      }
      if (dish[j][i] === '#') {
        curr = j + 1;
      }
    }
  }
  return dish;
}

const partOne = (input) =>
  calcDishLoad(tiltNorth(input.map(ln => ln.split(''))));

function partTwo(input) {
}

function soln(rawInput) {
  const input = rawInput.split('\n');
  fmtAnsWithRuntime(() => partOne(input), () => partTwo(input));
}

module.exports = { soln };
