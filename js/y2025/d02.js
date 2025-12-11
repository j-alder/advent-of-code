const { fmtAnsWithRuntime, freq } = require('../util.js');

const substrEq = (n, str) => {
  let prev = "";
  let i = 0;
  let eq = true;
  const m = str.length / n;
  while (eq && i < str.length) {
    const str_ = str.substring(i, i + m);
    eq = prev == "" || prev == str_;
    prev = str_;
    i += m;
  }
  return eq;
}

const partOne = (ranges) =>
  ranges.reduce((acc, range) => {
    const [start, end] = range;
    let x = start;
    while (x <= end) {
      x_ = x.toString();
      if (x_.length % 2 == 0 && substrEq(2, x_)) {
        acc += x
      }
      x++;
    }
    return acc;
  }, 0);

const allEq = (str) => {
  for (let i = 0; i < str.length - 1; i++) {
    if (str.charAt(i) !== str.charAt(i + 1)) {
      return false;
    }
  }
  return true;
}

const partTwo = (ranges) =>
  ranges.reduce((acc, range) => {
    const [start, end] = range;
    let x = start;
    for (let x = start; x <= end; x++) {
      x_ = x.toString();
      let ugh = 0;
      for (let y = 2; y < x_.length - 1; y++) {
        if (x_.length % y == 0 && substrEq(y, x_)) {
          ugh = x;
          break;
        }
      }
      if (ugh == 0 && allEq(x_)) {
        ugh = x;
      }
      acc += ugh;
    }
    return acc;
  }, 0);

function soln(rawInput) {
  const input = rawInput.split(',')
    .map((str) => str.split('-').map(Number));

  // 31755323542 is incorrect

  fmtAnsWithRuntime(() => partOne(input), () => partTwo(input));
}

module.exports = { soln };
