const process = require('process');
const { readInput } = require('./util');

const year = process.argv[2];
const day = process.argv[3];
const test = process.argv[4];

try {
  const input = readInput(year, day, test);
  const { soln } = require(`./y${year}/d${day < 10 ? `0${day}` : day}.js`);
  soln(input);
} catch (e) {
  console.error(e);
}
