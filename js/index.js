const process = require('process');

function readInput(year, day, test) {
  const fs = require('node:fs');
  const file = fs.readFileSync(`../input/y${year}/d${day < 10 ? `0${day}` : day}${test ? '.test' : ''}.txt`, 'utf8');
  return file;
}

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
