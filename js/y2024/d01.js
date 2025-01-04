const { fmtAnsWithRuntime, freq } = require('../util.js');

function partOne([list1, list2]) {
  const sortedList1 = list1.sort();
  const sortedList2 = list2.sort();
  return sortedList1.reduce((acc, locId, idx) => acc + Math.abs(locId - sortedList2[idx]), 0);
}

function partTwo([list1, list2]) {
  const list2Occ = freq(list2);
  return list1.reduce((acc, elem) => acc + ((list2Occ[elem] ?? 0) * elem), 0);
}

function soln(rawInput) {
  const input = rawInput.split('\n').reduce((acc, ln) => {
    const vals = ln.split("   ");
    acc[0].push(Number(vals[0]));
    acc[1].push(Number(vals[1]));
    return acc;
  }, [[], []]);
  fmtAnsWithRuntime(() => partOne(input), () => partTwo(input));
}

module.exports = { soln };
