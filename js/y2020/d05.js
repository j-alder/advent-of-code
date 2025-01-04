const { fmtAnsWithRuntime } = require('../util.js');

function calcSeatId(boardingPass) {
  if (boardingPass.length !== 10) return -1;
  let [minRow, maxRow] = [0, 127];
  let [minCol, maxCol] = [0, 7];
  boardingPass.split('').forEach((char, i) => {
    if (i <= 6) {
      if (char === 'F') maxRow = Math.floor((minRow + maxRow) / 2);
      else minRow = Math.ceil((minRow + maxRow) / 2);
    } else {
      if (char === 'L') maxCol = Math.floor((minCol + maxCol) / 2);
      else minCol = Math.ceil((minCol + maxCol) / 2);
    }
  });
  return minRow * 8 + minCol;
}

function partOne(input) {
  let seatId = -1;
  input.forEach(boardingPass => {
    seatId = Math.max(seatId, calcSeatId(boardingPass));
  });
  return seatId;
}

function partTwo(input) {
  const seats = input
    .reduce((result, boardingPass) => {
        const seatId = calcSeatId(boardingPass);
        if (seatId > -1) result.push(calcSeatId(boardingPass));
        return result;
      }, [])
    .sort((a, b) => a - b);
  for (let i = 0; i < seats.length - 1; i++) {
    if (seats[i+1] - seats[i] > 1) return seats[i] + 1;
  }
}

function soln(rawInput) {
  const input = rawInput.split('\n');
  fmtAnsWithRuntime(() => partOne(input), () => partTwo(input));
}

module.exports = { soln };
