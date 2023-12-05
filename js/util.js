function fmtSoln(ansOne, ansTwo) {
  console.log(`part one: ${ansOne ?? 'incomplete'}\npart two: ${ansTwo ?? 'incomplete'}`);
}

function fmtSolnWithRuntime(cb1, cb2) {
  const start = performance.now();
  const ansOne = cb1();
  const ansTwo = cb2();
  fmtSoln(ansOne, ansTwo);
  console.log(`runtime: ${performance.now() - start} ms`);
}

function readInput(year, day, test) {
  console.log(test);
  const fs = require('node:fs');
  const file = fs.readFileSync(`../input/y${year}/d${day < 10 ? `0${day}` : day}${test ? '_test' : ''}.txt`, 'utf8');
  return file;
}

module.exports = {
  fmtSoln,
  fmtSolnWithRuntime,
  readInput,
};

