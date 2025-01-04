const { fmtAnsWithRuntime } = require("../util.js");

function partOne(input) {
  let a = 0;
  let b = input.length - 1;
  let i = 0;
  let n = input[b];
  let total = 0;
  while (a < b) {
    if (a % 2 == 0) {
      for (let x = 0; x < input[a]; x++) {
        total += i++ * (a / 2);
      }
    } else {
      for (let x = 0; x < input[a]; x++) {
        if (n == 0 && b - a <= 1) break;
        if (n == 0) {
          b -= 2;
          n = input[b];
        }
        n--;
        total += i++ * (b / 2);
      }
    }
    a++;
  }
  while (n > 0) {
    total += i++ * (b / 2);
    n--;
  }
  return total;
}

function partTwo(input) {
  const drive = [];
  for (let i = 0; i < input.length; i++) {
    if (i % 2 == 0) {
      let val = [];
      for (let j = 0; j < input[i]; j++) {
        val.push(i / 2);
      }
      drive.push(val);
    } else {
      drive.push(input[i]);
    }
  }
  let b = drive.length - 1;
  while (b > 0) {
    let a = 0;
    while (a < b) {
      if (typeof drive[a] === "number" && drive[a] >= drive[b].length) {
        const x = [...drive[b]];
        drive[b] = x.length;
        drive[a] = drive[a] - x.length;
        drive.splice(a, 0, x);
        break;
      }
      a++;
    }
    b--;
  }
  let idx = 0;
  let total = 0;
  for (let i = 0; i < drive.length; i++) {
    if (typeof drive[i] === "number") {
      idx += drive[i];
    } else {
      for (const elem of drive[i]) {
        total += idx++ * elem;
      }
    }
  }
  return total;
}

function soln(rawInput) {
  const input = rawInput.split("").map(Number);
  fmtAnsWithRuntime(
    () => partOne(input),
    () => partTwo(input)
  );
}

module.exports = { soln };
