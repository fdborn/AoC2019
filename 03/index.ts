import { map, flatMap, filter, intersectionWith, isEqual } from 'lodash';
import input from './input.txt';

enum Direction {
  Up = 'U',
  Right = 'R',
  Down = 'D',
  Left = 'L',
}

interface Instruction {
  direction: Direction
  steps: number,
}

type Point = [number, number];

function parseInstruction(instruction: string): Instruction {
  const directionChar = instruction.charAt(0);
  const steps = parseInt(instruction.slice(1));

  return ({
    direction: <Direction>directionChar,
    steps,
  });
}

function* unrollInstruction(start: Point, { direction, steps }: Instruction) {
  let stepFn: (point: Point) => Point;
  let buffer: Point = start;

  switch(direction) {
    case Direction.Up:
      stepFn = ([x, y]: Point) => [x, y + 1];
      break;
    case Direction.Right:
      stepFn = ([x, y]: Point) => [x + 1, y];
      break;
    case Direction.Down:
      stepFn = ([x, y]: Point) => [x, y - 1];
      break;
    case Direction.Left:
      stepFn = ([x, y]: Point) => [x - 1, y];
      break;
  }

  for (let i = 0; i < steps; i++) {
    buffer = stepFn(buffer);
    yield buffer;
  }
}

function getDistance([aX, aY]: Point, [bX, bY]: Point): number {
  return Math.abs(aX - bX) + Math.abs(aY - bY);
}

// Kinda arbitrary
const centralPort: Point = [0, 0];

const inputLines = filter(input.split('\n'), line => line.length != 0);

const parsedWires = map(inputLines, line => {
  const instructions = filter(line.split(','), instruction => instruction.length != 0);

  return map(instructions, parseInstruction);
});

const interpolatedPoints = map(parsedWires, instructions => {
  let start = centralPort;

  return flatMap(instructions, instruction => {
    const points = [...unrollInstruction(start, instruction)];
    start = points[points.length - 1];

    return points;
  })
});

const intersections = intersectionWith(...interpolatedPoints, <any>isEqual);
const distances = map(intersections, (intersection: Point) => getDistance(centralPort, intersection));

const output1 = distances.sort()[0];
const output2 = 0;

export {
  output1 as solutionPartOne,
  output2 as solutionPartTwo,
};
