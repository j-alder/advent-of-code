const { fmtAnsWithRuntime } = require("../util.js");

/**
 * Determine if the pages of an order are in the right order.
 *
 * @param {Array<number>} order
 * @param {string} inst
 */
const isCorrect = (order, inst) =>
  order.every((page, pageIdx) => {
    if (pageIdx == page.length - 1) {
      return true;
    }
    const matchingNumbers = [
      ...inst.matchAll(new RegExp(`${page}\\|(\\d+)`, "g")),
    ];
    if (matchingNumbers.length > 0) {
      return matchingNumbers.every(([_, match]) => {
        const idx = order.findIndex((p) => p == Number(match));
        return idx == -1 || idx > pageIdx;
      });
    }
    return true;
  });

/**
 *
 * @param {Array<number>} order
 * @param {string} inst
 */
function reOrder(order, inst) {
  let newOrder = [...order];
  let changed = true;
  while (changed) {
    changed = false;
    for (let i = 0; i < newOrder.length; i++) {

      const numbersBefore = [
        ...inst.matchAll(new RegExp(`(\\d+)\\|${newOrder[i]}`, "g")),
      ].map((m) => Number(m[1]));

      for (const n of numbersBefore) {
        const nx = newOrder.indexOf(n);
        if (nx > i) {
          const num = newOrder.splice(nx, 1)[0];
          newOrder.splice(i, 0, num);
          changed = true;
        }
      }

      const numbersAfter = [
        ...inst.matchAll(new RegExp(`${newOrder[i]}\\|(\\d+)`, "g")),
      ].map((m) => Number(m[1]));

      for (const n of numbersAfter) {
        const nx = newOrder.indexOf(n);
        if (nx !== -1 && nx < i) {
          const num = newOrder.splice(nx, 1)[0];
          newOrder.splice(i + 1, 0, num);
          changed = true;
        }
      }
    }
  }
  return newOrder;
}

const partOne = (inst, orders) =>
  orders.reduce((total, order) => {
    if (isCorrect(order, inst)) {
      return total + order[Math.floor(order.length / 2)];
    }
    return total;
  }, 0);

const partTwo = (inst, orders) =>
  orders.reduce((total, order) => {
    if (!isCorrect(order, inst)) {
      const newOrder = reOrder(order, inst);
      return total + newOrder[Math.floor(newOrder.length / 2)];
    }
    return total;
  }, 0);

function soln(rawInput) {
  const [rawInstructions, rawOrders] = rawInput.split("\n\n");
  const orders = rawOrders.split("\n").map((ord) => ord.split(",").map(Number));
  fmtAnsWithRuntime(
    () => partOne(rawInstructions, orders),
    () => partTwo(rawInstructions, orders)
  );
}

module.exports = { soln };
