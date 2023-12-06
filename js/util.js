function fmtSoln(ansOne, ansTwo) {
  console.log(`part one: ${ansOne ?? 'incomplete'}\npart two: ${ansTwo ?? 'incomplete'}`);
}

function withRuntime(cb) {
  const start = performance.now();
  return [cb(), performance.now() - start];
}

function fmtAnsWithRuntime(cb1, cb2) {
  const start = performance.now();
  const [ansOne, rtOne] = withRuntime(cb1)
  const [ansTwo, rtTwo] = withRuntime(cb2);
  console.log(`part one: ${ansOne ?? 'incomplete'}\n  runtime (ms): ${rtOne}\n`);
  console.log(`part two: ${ansTwo ?? 'incomplete'}\n  runtime (ms): ${rtTwo}`);
}

function readInput(year, day, test) {
  const fs = require('node:fs');
  const file = fs.readFileSync(`../input/y${year}/d${day < 10 ? `0${day}` : day}${test ? '_test' : ''}.txt`, 'utf8');
  return file;
}

module.exports = {
  fmtSoln,
  fmtAnsWithRuntime,
  readInput,
};

