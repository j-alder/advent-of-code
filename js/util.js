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

function readInput(year, day, test) {
  const fs = require('node:fs');
  const file = fs.readFileSync(`../input/y${year}/d${day < 10 ? `0${day}` : day}${test ? '_test' : ''}.txt`, 'utf8');
  return file;
}

const sum = (arr) => arr.reduce((s, n) => s + n, 0);

module.exports = {
  fmtAnsWithRuntime,
  readInput,
  sum,
};

