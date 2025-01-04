const { fmtAnsWithRuntime, lcm } = require('../util.js');

function partOne(inst, nodes) {
  let curr = 'AAA';
  let steps = 0;
  while (curr !== 'ZZZ') {
    curr = inst[steps % inst.length] === 'L' 
      ? nodes[curr][0] 
      : nodes[curr][1];
    steps++;
  }
  return steps;
}

/**
 * Find the number of steps it takes to get to an end node (one that ends
 * with 'Z') from a starting node.
 * 
 * @param start {string} name of starting node
 * @param inst {Array<string>} sequence of left/right instructions
 * @param nodes {Object<string, Array<string>>} map of node names to their left/right children
 */
function findCycleLength(start, inst, nodes) {
  let curr = start;
  let steps = 0;
  while (!curr.endsWith('Z')) {
    curr = inst[steps % inst.length] === 'L'
      ? nodes[curr][0]
      : nodes[curr][1];
    steps++;
  }
  return steps;
}

function partTwo(inst, nodes) {
  // find all starting nodes
  const startNodes = Object.keys(nodes).filter(node => node.endsWith('A'));
  // calculate number of steps it takes to get to an ending node from each starting node
  const cycleLengths = startNodes.map(node => findCycleLength(node, inst, nodes));
  // find the LCM of the cycle lengths of each starting node
  return cycleLengths.reduce(lcm, 0);
}

function fmtInput(rawInput) {
  const [inst, nodesRaw] = rawInput.split('\n\n');
  const nodes = nodesRaw.split('\n').reduce((tree, nodeAndChildren) => {
    const [node, children] = nodeAndChildren.split(' = ');
    return {
      ...tree,
      [node]: children.split(', ').map((child) => child.replace(/[()]/g, ''))
    };
  }, {});
  return [inst, nodes];
}

function soln(rawInput) {
  const [inst, nodes] = fmtInput(rawInput);
  fmtAnsWithRuntime(() => partOne(inst, nodes), () => partTwo(inst, nodes));
}

module.exports = { soln };
