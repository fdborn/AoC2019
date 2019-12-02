import { map, filter, reduce } from 'lodash';
import input from './input.txt';

const rawLines = filter(input.split("\n"), input => input.length != 0);
const masses = map(rawLines, line => parseInt(line));

// Part one

function calculateFuel(mass: number): number {
  return Math.floor(mass / 3) - 2;
}

const output1 = reduce(masses, (acc, mass) => acc + calculateFuel(mass), 0);

// Part two

function calculateFuelRecursive(mass: number): number {
  const fuel = calculateFuel(mass);

  if (fuel <= 0) {
    return 0;
  }

  return fuel + calculateFuelRecursive(fuel);
}

const output2 = reduce(masses, (acc, mass) => acc + calculateFuelRecursive(mass), 0);

export {
  output1 as solutionPartOne,
  output2 as solutionPartTwo,
};
