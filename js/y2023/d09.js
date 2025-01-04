const { fmtAnsWithRuntime, sum } = require('../util.js');

function calcDeltas(history) {
  const deltas = [history];
  while (deltas[deltas.length - 1].some(n => n !== 0)) {
    const next = [];
    const last = deltas[deltas.length - 1];
    for (let i = 1; i < last.length; i++) {
      next.push(last[i] - last[i - 1]);
    }
    deltas.push(next);
  }
  return deltas.filter(it => it.length > 0);
}

const fmtDeltas = (lns) => 
  lns.map(ln => ln
      .split(' ')
      .map(Number))
    .map(calcDeltas);

function nextNumber(deltas) {
  let curr = 0;
  for (let i = deltas.length - 1; i >= 0; i--) {
    curr += deltas[i][deltas[i].length - 1];
  }
  return curr;
};

function prevNumber(deltas) {
  let curr = 0;
  for (let i = deltas.length - 1; i >= 0; i--) {
    curr = deltas[i][0] - curr;
  }
  return curr;
}

const partOne = (deltas) => sum(deltas.map(nextNumber));

const partTwo = (deltas) => sum(deltas.map(prevNumber));

function soln(rawInput) {
  const deltas = fmtDeltas(rawInput.split('\n'));
  fmtAnsWithRuntime(() => partOne(deltas), () => partTwo(deltas));
}

module.exports = { soln };
