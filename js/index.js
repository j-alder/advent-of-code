const process = require('process');
const { readInput } = require('./util');

const year = process.argv[2];
const day = process.argv[3];

try {
  const input = readInput(year, day);
  const { soln } = require(`./y${year}/d${day < 10 ? `0${day}` : day}.js`);
  soln(input);
} catch (e) {
  console.error(e);
}
