const { fmtAnsWithRuntime } = require('../util.js');

function updateCurrDir(currDir, turn) {
  let newDir = currDir;
  switch (turn) {
    case 'R':
      switch (currDir) {
        case 'N':
          newDir = 'E';
          break;
        case 'S':
          newDir = 'W';
          break;
        case 'E':
          newDir = 'S';
          break;
        case 'W':
          newDir = 'N';
          break;
        default:
          break;
      }
      break;
    case 'L':
      switch (currDir) {
        case 'N':
          newDir = 'W';
          break;
        case 'S':
          newDir = 'E';
          break;
        case 'E':
          newDir = 'N';
          break;
        case 'W':
          newDir = 'S';
          break;
        default:
          break;
      }
      break;
    default:
      break;
  }
  return newDir;
}

function updatePos(pos, to, blocks) {
  const newPos = {...pos};
  let from = to;
  switch (to) {
    case 'N':
      from = 'S';
      break;
    case 'S':
      from = 'N';
      break;
    case 'E':
      from = 'W';
      break;
    case 'W':
      from = 'E';
      break;
    default:
      break;
  }
  if (pos[from] > 0) {
    let diff = pos[from] - blocks;
    if (diff >= 0) {
      newPos[from] = diff;
    } else {
      newPos[from] = 0;
      newPos[to] = Math.abs(diff);
    }
  } else {
    newPos[to] += blocks;
  }

  return newPos;
}

function partOne(instructions) {
  let currDir = 'N';
  let pos = {
    N: 0,
    S: 0,
    E: 0,
    W: 0
  };
  instructions.forEach(([turn, blocks]) => {
    currDir = updateCurrDir(currDir, turn);
    pos = updatePos(pos, currDir, blocks);
  });
  return (pos.N + pos.S + pos.E + pos.W);
}

function partTwo(instructions) {
  let currDir = 'N';
  let pos = {
    N: 0,
    S: 0,
    E: 0,
    W: 0
  };
  const locs = {};
  for (const inst of instructions) {
    const entries = Object.entries(pos);
    const loc = entries.map(it => it.join('')).join('');
    if (locs[loc] != null) {
      return entries.reduce((acc, it) => acc + it[1], 0);
    } else {
      locs[loc] = 0;
    }
    currDir = updateCurrDir(currDir, inst[0]);
    pos = updatePos(pos, currDir, inst[1]);
  }
}

function soln(rawInput) {
  const input = rawInput.split(',');
  const instructions = input.map(inst => {
    const x = inst.trim().split('');
    return [x[0], Number(x.slice(1).join(''))]
  });
  fmtAnsWithRuntime(() => partOne(instructions), () => partTwo(instructions));
}

module.exports = { soln };

/*
--- Day 1: No Time for a Taxicab ---

Santa's sleigh uses a very high-precision clock to guide its movements, and 
the clock's oscillator is regulated by stars. Unfortunately, the stars have 
been stolen... by the Easter Bunny. To save Christmas, Santa needs you to 
retrieve all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each 
day in the Advent calendar; the second puzzle is unlocked when you complete 
the first. Each puzzle grants one star. Good luck!

You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near", 
unfortunately, is as close as you can get - the instructions on the Easter 
Bunny Recruiting Document the Elves intercepted start here, and nobody had 
time to work them out further.

The Document indicates that you should start at the given coordinates (where 
you just landed) and face North. Then, follow the provided sequence: either 
turn left (L) or right (R) 90 degrees, then walk forward the given number of 
blocks, ending at a new intersection.

There's no time to follow such ridiculous instructions on foot, though, so 
you take a moment and work out the destination. Given that you can only walk 
on the street grid of the city, how far is the shortest path to the 
destination?

For example:

  Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
  R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away.
  R5, L5, R5, R3 leaves you 12 blocks away.

How many blocks away is Easter Bunny HQ?

--- Part Two ---

Then, you notice the instructions continue on the back of the Recruiting 
Document. Easter Bunny HQ is actually at the first location you visit twice.

For example, if your instructions are R8, R4, R4, R8, the first location you 
visit twice is 4 blocks away, due East.

How many blocks away is the first location you visit twice?
*/
