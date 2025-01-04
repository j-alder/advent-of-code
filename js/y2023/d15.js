const { fmtAnsWithRuntime } = require('../util.js');

const hashStep = (input) => 
  input.reduce((acc, char) => 
    ((acc + char.charCodeAt(0)) * 17) % 256, 0);

const partOne = (input) => 
  input.reduce((acc, step) => 
    acc + hashStep(step), 0);

const countBox = (box, boxNum) => box.reduce((acc, lens, slot) =>
  acc + (boxNum + 1) * (slot + 1) * Number(lens.split(' ')[1]), 0);

function partTwo(input) {
  const boxes = Array(256).fill().map(() => []);
  input.split(',').forEach(step => {
    const [label, focalLength] = step.split(/[-=]/);
    const lens = `${label} ${focalLength}`;
    const op = step.match(/[-=]/)[0];
    const box = hashStep(label);
    if (op === '-') {
      const lensIdx = boxes[box].findIndex(l => l.split(' ')[0] === label);
      if (lensIdx > -1) {
        boxes[box].splice(lensIdx, 1);
      }
    } else {
      const lensIdx = boxes[box].findIndex(l => l.split(' ')[0] === label);
      if (lensIdx === -1) {
        boxes[box].push(lens);
      } else {
        boxes[box][lensIdx] = lens;
      }
    }
  });
  return boxes.reduce((acc, box, boxNum) =>
    acc + countBox(box, boxNum), 0);
}

function soln(rawInput) {
  const input = rawInput.split(',');
  fmtAnsWithRuntime(() => partOne(input), () => partTwo(input));
}

module.exports = { soln };
