const { fmtAnsWithRuntime } = require('../util.js');

const nPad = new Map([
  ["7", [0,0]],
  ["8", [0,1]],
  ["9", [0,2]],
  ["4", [1,0]], 
  ["5", [1,1]],
  ["6", [1,2]],
  ["1", [2,0]],
  ["2", [2,1]], 
  ["3", [2,2]],
  ["0", [3,1]],
  ["A", [3,2]]
]);

const dPad = new Map([
  ["^", [0,1]],
  ["A", [0,2]],
  ["<", [1,0]],
  ["v", [1,1]],
  [">", [1,2]]
]);

function getMovement([sx, sy], [ex, ey]) {
  const xdiff = sx - ex;
  const ydiff = sy - ey;
  let vc;
  if (xdiff > 0) {
    vc = "^";
  }
  if (xdiff < 0) {
    vc = "v";
  }
  let hc;
  if (ydiff > 0) {
    hc = "<";
  }
  if (xdiff < 0) {
    hc = ">";
  }
  return {
    v: vc ? [vc, Math.abs(xdiff)] : null,
    h: hc ? [hc, Math.abs(ydiff)] : null
  };
}

function permutations(permutationLength, alphabet) {
  console.log(permutationLength, alphabet);
  const result = [];

  function generate(current = "") {
    if (current.length === permutationLength) {
      result.push(current);
      return;
    }

    for (const a of alphabet) {
      generate(current + a);
    }
  }

  generate();
  return result;
}

function buttonPressPermutations(start, end, buttonMap) {
  const validCoords = new Set(buttonMap.values());
  const m = getMovement(start, end);
  console.log(m);
  const valpha = m.v && m.v[0].repeat(m.v[1]);
  const halpha = m.h && m.h[0].repeat(m.h[1]);
  console.log(halpha);

  if (m.v == null) {
    return [valpha];
  }

  if (m.h == null) {
    return [halpha];
  }

  const p = permutations(m.v[1] + m.h[1], valpha.split("").concat(halpha.split("")));

  return p.filter((perm) => {
    let [cx, cy] = [start[0], start[1]];
    for (const c of perm) {
      switch (c) {
        case "^":
          cx = cx - 1
        case "v":
          cx = cx + 1
        case ">":
          cy = cy + 1
        case "<":
          cy = cy - 1
      }
      if (validCoords.has(`${[cx,cy]}`)) return false;
    }
    return true;
  });
}

function partOne(inputs) {
  for (const input of inputs) {
    const inputQueue = input.split("");
    let currButton = "A";
    let nextButton = inputQueue.shift();
    while (nextButton != null) {
      const a = nPad.get(currButton);
      const b = nPad.get(nextButton);
      console.log(buttonPressPermutations(a, b, nPad));
      currButton = nextButton;
      nextButton = inputQueue.shift();
    }
  }

  // to press 7 from zero:
  // 0 -> 7
  // 3,1 -> 0,0
  // 3 -3 = ^^^
  // 1 -1 = <
  // second robot presses: ^<^^A ^^<^A ^^^<A
  // ^ -> A >
  // < -> A >>^ >^>
  // third robot presses: ^<^^(>)A ^^<^(>)A ^^^<(>>^)A ^^^<(>^>)A
  // 
}

function partTwo(input) {
}

function soln(rawInput) {
  const input = rawInput.split("\n");
  fmtAnsWithRuntime(
    () => partOne(input),
    () => partTwo(input)
  );
}

module.exports = { soln };
