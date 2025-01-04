const { fmtAnsWithRuntime } = require('../util.js');

const requiredFields = ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'];

const fieldRegex = /(byr)|(iyr)|(eyr)|(hgt)|(hcl)|(ecl)|(pid)/g;

const fieldAndValueRegex = /((byr|iyr|eyr):\d{4}[\s\n])|(hgt:(\d{2}in|\d{3}cm))|(hcl:#[0-9a-f]{6}[\s\n])|(ecl:(amb|blu|brn|gry|grn|hzl|oth))|(pid:\d{9}[\s\n])/gm;

const hasAllRequiredFields = (set) => 
  requiredFields.every(field => set.has(field));

function partOne(input) {
  let count = 0;
  input.forEach(passport => {
    const matches = new Set(passport.match(fieldRegex));
    if (hasAllRequiredFields(matches)) {
      count++;
    }
  });
  return count;
}

function correctNumber(numStr, lowerBound, upperBound) {
  try {
    return Number(numStr) >= lowerBound && Number(numStr) <= upperBound;
  } catch {
    return false;
  }
}

function partTwo(input) {
  let count = 0;
  input.forEach(passport => {
    const matches = passport
      .match(fieldAndValueRegex)
      .reduce((result, match) => {
        const [pre, suf] = match.trim(' ').trim('\n').split(':');
        switch (pre) {
          case 'byr':
            if (correctNumber(suf, 1920, 2002)) result.push(pre);
            break;
          case 'iyr':
            if (correctNumber(suf, 2010, 2020)) result.push(pre);
            break;
          case 'eyr':
            if (correctNumber(suf, 2020, 2030)) result.push(pre);
            break;
          case 'hgt':
            if (suf.substring(suf.length - 2) === 'in') {
              if (correctNumber(suf.substring(0, suf.length-2), 59, 76)) result.push(pre);
            } else {
              if (correctNumber(suf.substring(0, suf.length-2), 150, 193)) result.push(pre);
            }
            break;
          case 'ecl':
          case 'hcl':
          case 'pid':
          case 'cid':
            result.push(pre);
            break;
          default:
            break;
        }
        return result;
      }, []);
    if (hasAllRequiredFields(new Set(matches))) count++;
  });
  return count;
}

function soln(rawInput) {
  const input = rawInput.split('\n\n');
  fmtAnsWithRuntime(() => partOne(input), () => partTwo(input));
}

module.exports = { soln };
