const { fmtAnsWithRuntime } = require('../util.js');
const { md5 } = require("../util/md5.js");

/**
 * @param {string} firstChars 
 */
function findN(prefix, firstChars) {
  let n = 0;
  let hash = "";
  while (!hash.startsWith(firstChars)) {
    n++;
    hash = md5(`${prefix}${n}`);
  }
  return n;
}

/**
 * @param {string} rawInput
 */
function partOne(input) {
  return findN(input, "00000");
}

/**
 * @param {string} rawInput
 */
function partTwo(input) {
  return findN(input, "000000");
}

function soln(rawInput) {
  fmtAnsWithRuntime(
    () => partOne(rawInput),
    () => partTwo(rawInput)
  );
}

module.exports = { soln };
