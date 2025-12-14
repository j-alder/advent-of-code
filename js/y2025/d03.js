const { fmtAnsWithRuntime } = require('../util.js');

const getJolts = (batteries) => {
  // if (batteries.length === 0) {
  //   console.log(Number(n + m));
  //   return Number(n + m);
  // }
  // const x = batteries.pop();
  // if (m === undefined) return getJolts(batteries, [n, x]);
  // if (n === undefined) return getJolts(batteries, [x, m]);
  // if (x > n) {
  //   if (n > m) return getJolts(batteries, [x, n]);
  //   return getJolts(batteries, [x, m]);
  // }
  // return getJolts(batteries, [n, m]);
  const fstMax = Math.max(...batteries.slice(0, batteries.length - 1));
  const fstIdx = batteries.findIndex((x) => x == fstMax);
  const sndMax = Math.max(...batteries.slice(fstIdx + 1));
  return Number(fstMax * 10 + sndMax);
}

const partOne = (lns) => lns.reduce((acc, ln) => acc + getJolts([...ln]), 0);

const partTwo = () => {

}

function soln(rawInput) {
  const input = rawInput.split('\n');
  // part 1: 17554
  fmtAnsWithRuntime(() => partOne(input, 0), () => partTwo(input));
}

module.exports = { soln };
