const { fmtAnsWithRuntime } = require("../util.js");

const comboOperand = (operand, registers) => {
  switch (operand) {
    case 4:
      return registers.a;
    case 5:
      return registers.b;
    case 6:
      return registers.c;
    default:
      return operand;
  }
};

function evaluate(opcode, operand, registers) {
  switch (opcode) {
    case 0:
      registers.a = Math.floor(
        registers.a / Math.pow(2, comboOperand(operand, registers))
      );
      break;
    case 1:
      registers.b = registers.b ^ operand;
      break;
    case 2:
      registers.b = comboOperand(operand, registers) % 8;
      break;
    case 3:
      break;
    case 4:
      registers.b = (registers.b ^ registers.c) >>> 0;
      break;
    case 5:
      return comboOperand(operand, registers) % 8;
    case 6:
      registers.b = Math.floor(
        registers.a / Math.pow(2, comboOperand(operand, registers))
      );
      break;
    case 7:
      registers.c = Math.floor(
        registers.a / Math.pow(2, comboOperand(operand, registers))
      );
      break;
  }
}

function execute(registers, program) {
  let inst = 0;
  const output = [];
  while (inst < program.length - 1) {
    if (program[inst] === 3) {
      if (registers.a !== 0) {
        inst = program[inst + 1];
      } else {
        inst += 2;
      }
    } else {
      const res = evaluate(program[inst], program[inst + 1], registers);
      if (res != null) {
        output.push(res);
      }
      inst += 2;
    }
  }
  return output.join(",");
}

function partOne(inputRegisters, program) {
  const registers = structuredClone(inputRegisters);
  return execute(registers, program);
}

function partTwo(program) {
}

function soln(rawInput) {
  const [rawReg, rawProg] = rawInput.split("\n\n");
  const [a, b, c] = [...rawReg.matchAll(/\d+/g)].map((m) => Number(m[0]));
  const program = rawProg.split(" ")[1].split(",").map(Number);
  fmtAnsWithRuntime(
    () => partOne({ a, b, c }, program),
    () => partTwo(program)
  );
}

module.exports = { soln };
