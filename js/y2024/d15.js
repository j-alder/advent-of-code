const { fmtAnsWithRuntime, coordsOf, allCoordsOf } = require("../util.js");

const directions = {
  "^": [-1, 0],
  "<": [0, -1],
  v: [1, 0],
  ">": [0, 1],
};

function pushLine([sx, sy], [dx, dy], map) {
  let [cx, cy] = [sx, sy];
  const result = [[map[cx][cy], cx, cy]];
  while (map[cx][cy] !== "." && map[cx][cy] !== "#") {
    cx += dx;
    cy += dy;
    result.push([map[cx][cy], cx, cy]);
  }
  return result;
}

function partOne(map, moves) {
  let move;
  let [cx, cy] = coordsOf("@", map);
  const mvs = [...moves];
  while ((move = mvs.shift()) != null) {
    const [dx, dy] = directions[move];
    const line = pushLine([cx, cy], [dx, dy], map);
    if (line[line.length - 1][0] === ".") {
      let s;
      while (line.length > 1) {
        s = line.pop();
        map[s[1]][s[2]] = line[line.length - 1][0];
        cx = s[1];
        cy = s[2];
      }
      s = line.pop();
      map[s[1]][s[2]] = ".";
    }
  }
  const boxPositions = allCoordsOf("O", map);
  return boxPositions.reduce((acc, [x, y]) => acc + (100 * x + y), 0);
}

function pushLines([sx, sy], [dx, dy], map) {
  let curr = [[sx, sy]];
  const result = {};
  while (
    !curr.some(([x, y]) => map[x][y] == "#") &&
    !curr.every(([x, y]) => map[x][y] == ".")
  ) {
    curr.forEach(([cx, cy]) => {
      if (result[cx]) {
        result[cx][cy] = map[cx][cy];
      } else {
        result[cx] = {
          [cy]: map[cx][cy],
        };
      }
    });
    curr = curr.flatMap(([x, y]) => {
      const [nx, ny] = [x + dx, y + dy];
      console.log("mapnxny",map[nx][ny])
      if (map[nx][ny] == "[") {
        return [
          [nx, ny],
          [nx, ny + 1],
        ];
      }
      if (map[nx][ny] == "]") {
        return [
          [nx, ny],
          [nx, ny - 1],
        ];
      }
      return [[nx, ny]];
    });
  }
  if (curr.some(([x, y]) => map[x][y] == "#")) return null;
  console.log("curr",curr);
  console.log("result",result);
  return result;
}

function partTwo(map, moves) {
  let move;
  let [cx, cy] = coordsOf("@", map);
  const mvs = [...moves];
  while ((move = mvs.shift()) != null) {
    const [dx, dy] = directions[move];
    if (dy !== 0) {
      console.log("moving left/right");
      const line = pushLine([cx, cy], [dx, dy], map);
      if (line[line.length - 1][0] === ".") {
        let s;
        while (line.length > 1) {
          s = line.pop();
          map[s[1]][s[2]] = line[line.length - 1][0];
          cx = s[1];
          cy = s[2];
        }
        s = line.pop();
        map[s[1]][s[2]] = ".";
      }
    } else {
      const lines = pushLines([cx, cy], [dx, dy], map);
      if (lines) {
        console.log("lines",lines);
        const xs = Object.keys(lines).sort();
        console.log("xs",xs);
        let x;
        while (xs.length > 1) {
          console.log(cx);
          x = dx == -1 ? xs.pop() : xs.shift();
          const ys = Object.keys(lines[x]);
          for (const y of ys) {
            map[x][y] = lines[x - 1]?.[y] ?? ".";
          }
          cx = x;
        }
        x = dx == -1 ? xs.pop() : xs.shift();
        let [y] = Object.keys(lines[x]);
        map[x][y] = ".";
      }
    }
  }
  console.log(map.map(it => it.join("")).join("\n"))
}

/**
 *
 * @param {string} rawInput
 */
function soln(rawInput) {
  const [rawMap, rawMoves] = rawInput.split("\n\n");
  const map = rawMap.split("\n").map((ln) => ln.split(""));
  const moves = rawMoves.replace(/\n/g, "").split("");
  const wideMap = rawMap.split("\n").map((ln) =>
    ln.split("").flatMap((ch) => {
      switch (ch) {
        case ".":
          return [".", "."];
        case "#":
          return ["#", "#"];
        case "O":
          return ["[", "]"];
        case "@":
          return ["@", "."];
      }
    })
  );
  fmtAnsWithRuntime(
    () => partOne(map, moves),
    () => partTwo(wideMap, moves)
  );
}

module.exports = { soln };
