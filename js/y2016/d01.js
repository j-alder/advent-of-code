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
  const locs = new Set();
  for (const [turn, blocks] of instructions) {
    currDir = updateCurrDir(currDir, turn);
    for (let i = 0; i < blocks; i++) {
      pos = updatePos(pos, currDir, 1);
      const loc = Object.values(pos).join('');
      if (locs.has(loc)) {
        return Object.values(pos).reduce((acc, it) => acc + it, 0)
      } else {
        locs.add(loc);
      }
    }
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
