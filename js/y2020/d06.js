const { fmtAnsWithRuntime } = require('../util.js');

const partOne = (input) =>
  input.reduce((total, ln) => 
    total + new Set(ln.match(/[a-z]{1}/g)).size, 0);

const countMap = (groups) =>
  groups.reduce((cntMap, qList) => {
    qList.forEach(q => {
      cntMap[q] = cntMap[q] ? cntMap[q] + 1 : 1;
    });
    return cntMap;
  }, {});

const partTwo = (input) => 
  input.reduce((total, ln) => {
    const groups = ln
      .split('\n')
      .map(grp => grp
        .match(/[a-z]{1}/g));
    const groupLen = groups.length;
    if (groupLen === 1) {
      return total + groups[0].length;
    }
    const ayes = 
      Object.values(countMap(groups))
        .reduce((ayes, c) => c === groupLen ? ayes + 1 : ayes, 0);
    return total + ayes;
  }, 0);

function soln(rawInput) {
  const input = rawInput.split('\n\n');
  fmtAnsWithRuntime(() => partOne(input), () => partTwo(input));
}

module.exports = { soln };
