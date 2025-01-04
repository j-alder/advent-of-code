const { fmtAnsWithRuntime } = require('../util.js');

const FIVE_OF_A_KIND = 70_000_000_000_000;
const FOUR_OF_A_KIND = 60_000_000_000_000;
const FULL_HOUSE = 50_000_000_000_000;
const THREE_OF_A_KIND = 40_000_000_000_000;
const TWO_PAIR = 30_000_000_000_000;
const TWO_OF_A_KIND = 20_000_000_000_000;
const HIGH_CARD = 10_000_000_000_000;

const cards = ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'];
const jokerCards = ['J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A'];

function fmtHand(handStr) {
  const hand = {};
  for (let card of handStr) {
    hand[card] = (hand[card] ?? 0) + 1
  }
  return hand;
}

/* --- PART 1 --- */

function scoreHandType(hand, jokers = false) {
  const counts = Object.values(jokers ? fmtHandWithJokers(hand) : fmtHand(hand));
  const alike = Math.max(...counts);
  if (alike == 5) return FIVE_OF_A_KIND;
  else if (alike == 4) return FOUR_OF_A_KIND;
  else if (counts.includes(3) && counts.includes(2)) return FULL_HOUSE;
  else if (alike == 3) return THREE_OF_A_KIND;
  else if (counts.filter(v => v === 2).length === 2) return TWO_PAIR;
  else if (alike == 2) return TWO_OF_A_KIND;
  else return HIGH_CARD;
}

const score = (card) => cards.indexOf(card) + 1;

const scoreHighCards = (hand) =>
  hand.split('').reduce((total, card, idx) => 
    total + score(card) * 100 ** (hand.length - idx), 0);

const scoreHand = (hand) => scoreHandType(hand) + scoreHighCards(hand);

const partOne = (input) => 
   input
     .sort(([handA], [handB]) => scoreHand(handA) - scoreHand(handB))
     .reduce((winnings, [_, bid], idx) => winnings + bid * (idx + 1), 0);

/* --- PART 2 --- */

function scoreHandTypeWithJokers(handStr) {
  const hand = fmtHand(handStr);
  let uniqueCards = Object.keys(hand);

  if (uniqueCards.length === 1) return FIVE_OF_A_KIND;

  const jokers = hand['J'];

  if (!jokers) return scoreHandType(handStr);

  delete hand['J'];
  uniqueCards = Object.keys(hand);
  const entries = Object.entries(hand);

  if (uniqueCards.length === 1) return FIVE_OF_A_KIND;

  if (uniqueCards.length === 2) {
    if (entries.every(([_, count]) => count === 2) && jokers === 1) {
      return FULL_HOUSE;
    } else {
      return FOUR_OF_A_KIND;
    }
  }

  if (uniqueCards.length === 3) return THREE_OF_A_KIND;

  if (uniqueCards.length === 4) return TWO_OF_A_KIND;
}

const scoreWithJokers = (card) => jokerCards.indexOf(card) + 1;

const scoreHighCardsWithJokers = (hand) =>
  hand.split('').reduce((total, card, idx) => 
    total + scoreWithJokers(card) * 100 ** (hand.length - idx), 0);

const scoreHandWithJokers = (hand) => scoreHandTypeWithJokers(hand) + scoreHighCardsWithJokers(hand);

const partTwo = (input) => 
  input
    .sort(([handA], [handB]) => scoreHandWithJokers(handA) - scoreHandWithJokers(handB))
    .reduce((winnings, [_, bid], idx) => winnings + bid * (idx + 1), 0);

const fmtInput = (input) =>
  input.split('\n').map(ln => {
    const lns = ln.split(' ');
    return [lns[0], Number(lns[1])];
  });

function soln(rawInput) {
  const input = fmtInput(rawInput);
  fmtAnsWithRuntime(() => partOne(input), () => partTwo(input));
}

module.exports = { soln };
