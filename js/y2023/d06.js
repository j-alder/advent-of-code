const { fmtAnsWithRuntime } = require('../util.js');

function countWays(raceLength, minDistance) {
  let count = 0;
  for (let i = 0; i < raceLength; i++) {
    let possibleDistance = i * (raceLength - i);
    if (possibleDistance > minDistance) count++;
  }
  return count;
}

const partOne = ([raceLengths, topScores]) =>
  raceLengths.reduce((total, raceLength, i) => 
    total * countWays(raceLength, topScores[i]), 1);

const partTwo = ([raceLength, minDistance]) => 
  countWays(raceLength, minDistance);

const fmtInput = (rawInput) => ([
  rawInput
    .split('\n')
    .map(ln => ln
      .split(':')[1]
      ?.match(/\d+/g)
      .map(Number))
    .filter(it => !!it),
  rawInput
    .split('\n')
    .map(ln => ln
      .split(':')[1]
      ?.match(/\d+/g)
      .join(''))
    .filter(it => !!it)
    .map(Number)
]);

function soln(rawInput) {
  const [input1, input2] = fmtInput(rawInput);
  fmtAnsWithRuntime(() => partOne(input1), () => partTwo(input2));
}

module.exports = { soln };
