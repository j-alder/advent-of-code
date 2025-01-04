const { fmtAnsWithRuntime } = require('../util.js');

const regex = /(\d* red|\d* green|\d* blue)/g;

const fmtGame = (input) => 
  input.reduce((result, game) => {
    const [gameIdStr, gameResultStr] = game.split(':');
    if (gameIdStr) {
      const gameId = gameIdStr.split(' ')[1];
      const gameResults = gameResultStr.split(';').map(res =>
        res.match(regex).reduce((gameResults, match) => {
          const [count, color] = match.split(' ');
          gameResults = { ...gameResults, [color]: parseInt(count) }
          return gameResults;
        }, {})
      );
      result[gameId] = gameResults;
    }
    return result;
  }, {});

const partOne = (games) => 
  Object.entries(games)
    .reduce((total, [id, results]) => {
      if (results.some(r => r.red > 12 || r.green > 13 || r.blue > 14)) {
        return total;
      }
      return total + parseInt(id);
    }, 0);

const partTwo = (games) =>
  Object.entries(games)
    .reduce((total, [id, results]) => {
      let r, g, b;
      results.forEach(res => {
        r = res.red ? Math.max(res.red, r ?? 1) : r;
        g = res.green ? Math.max(res.green, g ?? 1) : g;
        b = res.blue ? Math.max(res.blue, b ?? 1) : b;
      });
      return total + r * g * b;
    }, 0);

function soln(rawInput) {
  const input = rawInput.split('\n');
  const games = fmtGame(input);
  fmtAnsWithRuntime(() => partOne(games), () => partTwo(games));
}

module.exports = { soln };
