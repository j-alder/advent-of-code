const { rotateMatrixClockwise, rotateMatrixAntiClockwise } = require("./util");

describe('rotateMatrixAntiClockwise', () => {
  it('should rotate a 2x2 matrix appropriately', () => {
    const twoByTwo = [
      ['a', 'b'],
      ['c', 'd'],
    ];
    const expected = [
      ['b', 'd'],
      ['a', 'c'],
    ];
    expect(rotateMatrixAntiClockwise(twoByTwo)).toEqual(expected);
  });

  it('should rotate a 2x3 matrix appropriately', () => {
    const twoByThree = [
      ['a', 'b', 'c'],
      ['d', 'e', 'f'],
    ];
    const expected = [
      ['c', 'f'],
      ['b', 'e'],
      ['a', 'd'],
    ];
    expect(rotateMatrixAntiClockwise(twoByThree)).toEqual(expected);
  });
});

describe('rotateMatrixClockwise', () => {
  it('should rotate a 2x2 matrix appropriately', () => {
    const twoByTwo = [
      ['a', 'b'],
      ['c', 'd'],
    ];
    const expected = [
      ['c', 'a'],
      ['d', 'b'],
    ];
    expect(rotateMatrixClockwise(twoByTwo)).toEqual(expected);
  });

  it('should rotate a 2x3 matrix appropriately', () => {
    const twoByThree = [
      ['a', 'b', 'c'],
      ['d', 'e', 'f'],
    ];
    const expected = [
      ['d', 'a'],
      ['e', 'b'],
      ['f', 'c'],
    ];
    expect(rotateMatrixClockwise(twoByThree)).toEqual(expected);
  });
});
