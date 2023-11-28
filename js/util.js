function fmtSoln(ansOne, ansTwo) {
  console.log(`part one: ${ansOne ?? 'incomplete'}\npart two: ${ansTwo ?? 'incomplete'}`);
}

function readInput(year, day) {
  const fs = require('node:fs');
  const file = fs.readFileSync(`../input/y${year}/d${day < 10 ? `0${day}` : day}.txt`, 'utf8');
  return file;
}

module.exports = {
  fmtSoln,
  readInput,
};

