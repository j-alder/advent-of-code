const { fmtAnsWithRuntime } = require("../util.js");

function mutate(stone) {
  let result = [];
  if (stone === "0") result = ["1"];
  else if (stone.length % 2 === 0)
    result = [
      stone.substring(0, stone.length / 2),
      stone.substring(stone.length / 2),
    ];
  else result = [(Number(stone) * 2024).toString()];

  return result.map((it) => Number(it).toString());
}

function partOne(stones) {
  let s = [...stones];
  for (let i = 0; i < 25; i++) {
    s = s.flatMap((stone) => mutate(stone));
  }
  return s.length;
}

function partTwo(input) {
  const stoneCounts = Object.fromEntries(input.map((stone) => [stone, 1]));
  for (let i = 0; i < 75; i++) {
    for (const [stone, count] of Object.entries(stoneCounts)) {
      if (stoneCounts[stone] - count === 0) {
        delete stoneCounts[stone];
      } else {
        stoneCounts[stone] -= count;
      }
      const x = mutate(stone);
      x.forEach((it) => (stoneCounts[it] = (stoneCounts[it] ?? 0) + count));
    }
  }
  return Object.values(stoneCounts).reduce((acc, n) => acc + n, 0);
}

function soln(rawInput) {
  const input = rawInput.split(" ");
  fmtAnsWithRuntime(
    () => partOne(input),
    () => partTwo(input)
  );
}

module.exports = { soln };
