const { fmtAnsWithRuntime } = require('../util.js');

const getPoints = (winningNumbers, cardNumbers) => 
  cardNumbers.reduce((points, cardNumber) => {
    if (winningNumbers.has(cardNumber)) {
      return (points == 0 ? 1 : points * 2);
    }
    return points;
  }, 0);

const partOne = (cards) =>
  cards.reduce((total, card) => {
    return total + getPoints(new Set(card[0]), card[1]);
  }, 0);

const totalWinning = (winningNumbers, cardNumbers) => 
  cardNumbers.reduce((total, cardNumber) => {
    if (winningNumbers.has(cardNumber)) {
      return total + 1;
    }
    return total;
  }, 0);

const partTwo = (cards) =>
  cards.reduce((total, card, ci) => {
    const winningNumbers = new Set(card[0]);
    const cardNumbers = card[1];
    let totalScratchers = totalWinning(winningNumbers, cardNumbers);
    while (totalScratchers > 0) {
      if (totalScratchers + ci < cards.length) {
        cards[totalScratchers + ci][2] += 1 * card[2]
      }
      totalScratchers--;
    }
    return total + card[2];
  }, 0);

const fmtInput = rawInput => rawInput
  .split('\n')
  .filter(s => s !== '')
  .map(ln => ln
    .split(':')[1]
    .split('|')
    .map(it => it
      .match(/\d+/g)
      .map(n => Number(n))))
  .map(it => ([...it, 1]));

function soln(rawInput) {
  const cards = fmtInput(rawInput);
  fmtAnsWithRuntime(() => partOne(cards), () => partTwo(cards));
}

module.exports = { soln };
