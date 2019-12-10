import { map, filter, reduce } from 'lodash';
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

interface Point {
  x: number,
  y: number,
}

type Segment = [Point, Point];

type Line = Segment[];

function parseInstruction(instruction: string): Instruction {
  const directionChar = instruction.charAt(0);
  const steps = parseInt(instruction.slice(1));

  return ({
    direction: <Direction>directionChar,
    steps,
  });
}

function applyInstruction(start: Point, { direction, steps }: Instruction): Point {
  const destination = { x: start.x, y: start.y };

  switch (direction) {
    case Direction.Up:
      destination.y += steps;
      break;
    case Direction.Right:
      destination.x += steps;
      break;
    case Direction.Down:
      destination.y -= steps;
      break;
    case Direction.Left:
      destination.x -= steps;
      break;
  }


  return destination;
}

function createLine(start: Point, instructions: Instruction[]): Line {
  const seededLine: Segment[] =  reduce(instructions, (acc, instruction) => {
    const start = acc[acc.length - 1][1];

    return [...acc, [start, applyInstruction(start, instruction)]];
  }, [[start, start]]);

  return seededLine.splice(1);
}


function getDistance(a: Point, b: Point): number {
  return Math.abs(a.x - b.x) + Math.abs(a.y - b.y);
}

// line intercept math by Paul Bourke http://paulbourke.net/geometry/pointlineplane/
// Determine the intersection point of two line segments
// Return FALSE if the lines don't intersect
function intersect([a, b]: Segment, [c, d]: Segment): Point | false {

  // Check if none of the lines are of length 0
	if ((a.x === b.x && a.y === b.y) || (c.x === d.x && c.y === d.y)) {
		return false;
	}

	const denominator = ((d.y - c.y) * (b.x - a.x) - (d.x - c.x) * (b.y - a.y));

  // Lines are parallel
	if (denominator === 0) {
		return false;
	}

	const ua = ((d.x - c.x) * (a.y - c.y) - (d.y - c.y) * (a.x - c.x)) / denominator;
	const ub = ((b.x - a.x) * (a.y - c.y) - (b.y - a.y) * (a.x - c.x)) / denominator;

  // is the intersection along the segments
	if (ua < 0 || ua > 1 || ub < 0 || ub > 1) {
		return false;
	}

  // Return a object with the x and y coordinates of the intersection
	let x = a.x + ua * (b.x - a.x);
	let y = a.y + ua * (b.y - a.y);

	return { x, y } ;
}

function findIntersections(lineA: Line, lineB: Line) {
  let buffer: Point[] = [];

  for (const segmentA of lineA) {
    for (const segmentB of lineB) {
      const intersection = intersect(segmentA, segmentB);
      if (intersection) {
        buffer = [...buffer, intersection];
      }
    }
  }

  return filter(buffer, ({ x, y }) => !(x == 0 && y == 0));
}

function countStepsToPoint(from: Point, to: Point, instructions: Instruction[]): number {
  let current = Object.assign({}, from);
  let totalSteps = 0;

  for (const { direction, steps } of instructions) {
    for (let step = 0; step < steps; step++) {

      if (current.x == to.x && current.y == to.y) {
        return totalSteps;
      }

      switch (direction) {
        case Direction.Up:
          current.y += 1;
          break;
        case Direction.Right:
          current.x += 1;
          break;
        case Direction.Down:
          current.y -= 1;
          break;
        case Direction.Left:
          current.x -= 1;
          break;
      }

      totalSteps += 1;
    }
  }

  return NaN;
}

// Part one

// Kinda arbitrary
const centralPort = { x: 0, y: 0 };

const inputLines = filter(input.split('\n'), line => line.length != 0);
const parsedWires = map(inputLines, line => {
  const instructions = filter(line.split(','), instruction => instruction.length != 0);

  return map(instructions, parseInstruction);
});

const lines = map(parsedWires, wire => createLine(centralPort, wire));
const intersections = findIntersections(lines[0], lines[1]);
const distances = map(intersections, point => getDistance(centralPort, point));

const output1 = [...distances].sort((a, b) => a - b)[0];

// Part two

const firstIntersection = intersections[0];
const lineASteps = countStepsToPoint(centralPort, firstIntersection, parsedWires[0]);
const lineBSteps = countStepsToPoint(centralPort, firstIntersection, parsedWires[1]);

const output2 = lineASteps + lineBSteps;

export {
  output1 as solutionPartOne,
  output2 as solutionPartTwo,
};
