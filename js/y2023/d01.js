const { fmtAnsWithRuntime } = require('../util.js');

const partOne = (input) =>
  input.reduce((total, calibration) => {
    const digits = calibration.match(/\d{1}/g);
    return digits !== null ? total + Number(digits[0] + digits[digits.length-1]) : total;
  }, 0);

const dict = {
  one: '1',
  two: '2',
  three: '3',
  four: '4',
  five: '5',
  six: '6',
  seven: '7',
  eight: '8',
  nine: '9'
}

function getNumStr(match) {
  if (/\d/.test(match)) return match;
  return dict[match];
}

const partTwo = (input) =>
  input.reduce((total, calibration) => {
    const digits = Array.from(
      calibration.matchAll(/(?=(one|two|three|four|five|six|seven|eight|nine|\d))/g), 
      match => match[1]
    );
    return digits.length > 0 
      ? total + Number(getNumStr(digits[0]) + getNumStr(digits[digits.length-1])) 
      : total;
  }, 0);

function soln(rawInput) {
  const input = rawInput.split('\n');
  fmtAnsWithRuntime(() => partOne(input), () => partTwo(input));
}

module.exports = { soln };
