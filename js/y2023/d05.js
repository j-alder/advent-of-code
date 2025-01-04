const { fmtAnsWithRuntime } = require('../util.js');

function getLocation(seed, maps) {
  let ref = seed;
  for (let map of maps) {
    for (let conversion of map) {
      if (ref >= conversion[1] && ref <= conversion[1] + (conversion[2] - 1)) {
        ref = ref - (conversion[1] - conversion[0]);
        break;
      }
    }
  }
  return ref;
}

function partOne([seeds, maps]) {
  let closestLocation;
  seeds.forEach(seed => {
    const location = getLocation(seed, maps);
    if (!closestLocation || location < closestLocation) {
      closestLocation = location;
    }
  });
  return closestLocation;
}

function partTwo([seeds, maps]) {
  let closestLocation;
  for (let a = 0; a < seeds.length; a += 2) {
    for (let b = 0; b < seeds[a + 1]; b++) {
      const location = getLocation(seeds[a] + b, maps);
      if (!closestLocation || location < closestLocation) {
        closestLocation = location;
      }
    }
  }
  return closestLocation;
}

const fmtInput = (input) => ([
  input[0]
    .split(':')[1]
    .match(/(\d+)/gm)
    .map(it => Number(it)),
  input
    .slice(1)
    .map(it => it
      .split('\n')
      .slice(1)
      .map(s => s
        .split(' ')
        .map(n => Number(n)))
      .filter(m => m.length === 3))
]);

function soln(rawInput) {
  const input = rawInput.split('\n\n');
  const seedsAndMaps = fmtInput(input);
  fmtAnsWithRuntime(() => partOne(seedsAndMaps), () => partTwo(seedsAndMaps));
}

module.exports = { soln };
