const { fmtAnsWithRuntime } = require("../util.js");

function parseInput(input) {
  return input.split("\n").reduce((acc, connection) => {
    const [a, b] = connection.split("-");
    if (acc.has(a)) {
      acc.get(a).add(b);
    } else {
      acc.set(a, new Set([b]));
    }

    if (acc.has(b)) {
      acc.get(b).add(a);
    } else {
      acc.set(b, new Set([a]));
    }

    return acc;
  }, new Map());
}

function partOne(input) {
  const nodeGraph = parseInput(input);
  const result = new Set();
  for (const [k, v] of nodeGraph.entries()) {
    const cn = [...v];
    for (let i = 0; i < cn.length - 1; i++) {
      for (let j = i + 1; j < cn.length; j++) {
        if (nodeGraph.get(cn[j]).has(cn[i])) {
          const st = [k, cn[j], cn[i]].sort();
          if (st.some((n) => n.startsWith("t"))) {
            result.add(`${st}`);
          }
        }
      }
    }
  }
  return result.size;
}

function partTwo(input) {
  // Bron-Kerbosch?
}

function soln(rawInput) {
  fmtAnsWithRuntime(
    () => partOne(rawInput),
    () => partTwo(rawInput)
  );
}

module.exports = { soln };
