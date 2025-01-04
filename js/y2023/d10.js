const { fmtAnsWithRuntime, getNeighborsWithCoordinates, coordsOf } = require('../util.js');

class Node {
  constructor(x, y, type) {
    this.x = x;
    this.y = y;
    this.type = type;
  }
}

const validMoves = {
  S: {
    w: ['F', 'L', '-'],
    e: ['J', '7', '-'],
    n: ['|', 'F', '7'],
    s: ['|', 'J', 'L'],
  },
  '|': {
    w: [],
    e: [],
    n: ['|', 'F', '7', 'S'],
    s: ['|', 'J', 'L', 'S'],
  },
  '-': {
    w: ['-', 'F', 'L', 'S'],
    e: ['-', 'J', '7', 'S'],
    n: [],
    s: [],
  },
  F: {
    w: [],
    e: ['-', 'J', '7', 'S'],
    n: [],
    s: ['|', 'J', 'L', 'S'],
  },
  L: {
    w: [],
    e: ['J', '7', '-', 'S'],
    n: ['|', 'F', '7', 'S'],
    s: [],
  },
  J: {
    w: ['-', 'L', 'F', 'S'],
    e: [],
    n: ['|', 'F', '7', 'S'],
    s: [],
  },
  7: {
    w: ['-', 'L', 'F', 'S'],
    e: [],
    n: [],
    s: ['|', 'J', 'L', 'S'],
  },
  '.': {
    w: [],
    e: [],
    n: [],
    s: [],
  },
};

function isValidPipe(targetPipe, currPipe, dir, targetX, targetY, path) {
  if (!targetPipe) return false;
  if (!validMoves[currPipe][dir].includes(targetPipe)) return false;
  const prevNode = path[path.length - 2];
  if (!prevNode) return true;
  return !(prevNode.x === targetX && prevNode.y === targetY);
}

const getValidPipe = (node, matrix, path) => 
  Object.entries(getNeighborsWithCoordinates(node.x, node.y, matrix))
    .filter(([k, _]) => k === 'n' || k === 's' || k === 'e' || k === 'w')
    .find(([dir, val]) => isValidPipe(val[0], node.type, dir, val[1], val[2], path));

const getNextNode = (node, matrix, path) => {
  const validPipe = getValidPipe(node, matrix, path);
  if (!validPipe) return null;
  return new Node(validPipe[1][1], validPipe[1][2], validPipe[1][0]);
};

function partOne(root, matrix) {
  let steps = 0;
  let curr = root;
  let path = [curr];
  let neighbor = getNextNode(curr, matrix, path);
  while (neighbor.type !== 'S') {
    curr = neighbor;
    path.push(curr);
    steps++;
    neighbor = getNextNode(curr, matrix, path);
  }
  return Math.ceil(steps / 2);
}

function soln(rawInput) {
  const matrix = rawInput.split('\n').map(ln => ln.split(''));
  const startingCoords = coordsOf('S', matrix);
  const root = new Node(startingCoords[0], startingCoords[1], 'S');
  fmtAnsWithRuntime(() => partOne(root, matrix), () => partTwo(matrix));
}

module.exports = { soln };
