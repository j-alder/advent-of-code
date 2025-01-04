const { fmtAnsWithRuntime } = require("../util.js");

/*
backtrack from znn until x and y are found?
*/

/**
 *
 * @param {string} rawInput
 * @returns {{ initialState: Map<string, number>, instructions: Map<string, {fstWire: string; gate: string; sndWire: string; targetWire: string}>}}
 */
function parseInputOne(rawInput) {
  const [rawState, rawInst] = rawInput.split("\n\n");
  const initialState = new Map(
    rawState
      .split("\n")
      .map((ln) => ln.split(": "))
      .map(([wire, value]) => [wire, parseInt(value)])
  );
  const instructions = rawInst.split("\n").reduce((acc, ln, idx) => {
    const [fstWire, gate, sndWire, targetWire] = ln
      .replace("-> ", "")
      .split(" ");
    acc.set(idx, {
      fstWire,
      gate,
      sndWire,
      targetWire,
    });
    return acc;
  }, new Map());
  return {
    initialState,
    instructions,
  };
}

function execute(fstWireVal, gate, sndWireVal) {
  switch (gate) {
    case "AND":
      return fstWireVal && sndWireVal ? 1 : 0;
    case "OR":
      return fstWireVal || sndWireVal ? 1 : 0;
    case "XOR":
      return fstWireVal ^ sndWireVal;
  }
}

function partOne(input) {
  const { initialState: state, instructions } = parseInputOne(input);
  while (instructions.size > 0) {
    for (const [id, inst] of instructions.entries()) {
      if (state.has(inst.fstWire) && state.has(inst.sndWire)) {
        state.set(
          inst.targetWire,
          execute(state.get(inst.fstWire), inst.gate, state.get(inst.sndWire))
        );
        instructions.delete(id);
        break;
      }
    }
  }
  let total = 0;
  for (const [gate, value] of state.entries()) {
    if (gate.startsWith("z") && value == 1) {
      total += Math.pow(2, parseInt(gate.match(/\d+/)[0]));
    }
  }
  return total;
}

function parseInputTwo(rawInput) {
  const [_, rawInst] = rawInput.split("\n\n");
  const inst = rawInst.split("\n").reduce((acc, ln) => {
    const [k, v] = ln.split(" -> ");
    if (acc.has(k)) console.log("duplicate found!", k);
    acc.set(k, v);
    return acc;
  }, new Map());
  return inst;
}

function partTwo(input) {
  const inst = parseInputTwo(input);
  // create chain of input?
  const origins = [];
  for (const [k, v] of inst.entries()) {
    if ([...k.matchAll(/[xy]\d+/g)].length == 2) {
      const [fst, _, snd] = k.split(" ");
      if (!(fst.substring(1) == snd.substring(1) && v.substring(1) == snd.substring(1))) {
        origins.push([k, v]);
      }
    }
  } // 88 origin allocs
  const seconds = [];
  for (const [op, out] of origins) {
    for (const [k, v] of inst.entries()) {
      if (k.match(new RegExp(out))) {
        seconds.push([k, v])
      }
    }
  }
  const zs = [];
  const thirds = [];
  for (const [op, out] of seconds) {
    for (const [k, v] of inst.entries()) {
      if (out.startsWith("z")) {
        zs.push([op, out]);
      } else if (k.match(new RegExp(out))) {
        thirds.push([k, v])
      }
    }
  }
  console.log("origins",origins);
  console.log("seconds",seconds);
  console.log("zs",zs);
  console.log("thirds",thirds);
}

function soln(rawInput) {
  fmtAnsWithRuntime(
    () => partOne(rawInput),
    () => partTwo(rawInput)
  );
}

module.exports = { soln };
