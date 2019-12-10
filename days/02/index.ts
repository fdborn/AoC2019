import { map } from 'lodash';
import input from './input.txt';

type Intcode = number[];
type InterpreterState = {
  memory: Intcode,
  headPosition: number,
  running: boolean,
}

const initialState = map(input.replace('\n', '').split(','), code => parseInt(code));

// Part one

function interpret(code: Intcode): Intcode {
  let state = {
    memory: code,
    headPosition: 0,
    running: true,
  };

  while (state.running) {
    state = transitionState(state);
  }

  return state.memory;
}

function transitionState({ memory, headPosition }: InterpreterState): InterpreterState {
  const buffer = Object.assign({}, memory);

  const opcode = memory[headPosition];
  let newHeadOffset = 0;
  let terminates = false;
  let lhs, rhs, outPos, result;

  switch (opcode) {
    case 1:
      newHeadOffset = 4;
      lhs = memory[memory[headPosition + 1]];
      rhs = memory[memory[headPosition + 2]];
      outPos = memory[headPosition + 3];

      result = lhs + rhs;
      buffer[outPos] = result;
      break;
    case 2:
      newHeadOffset = 4;
      lhs = memory[memory[headPosition + 1]];
      rhs = memory[memory[headPosition + 2]];
      outPos = memory[headPosition + 3];

      result = lhs * rhs;
      buffer[outPos] = result;
      break;
    case 99:
    default:
      terminates = true;
  }

  return {
    memory: buffer,
    headPosition: headPosition += newHeadOffset,
    running: !terminates,
  };
}

const partOneState = [...initialState];
partOneState[1] = 12;
partOneState[2] = 2;

const output1 = interpret(partOneState)[0];

// Part two

function findMatchingWords(code: Intcode , target: number): { noun: number, verb: number } {
  for (let noun = 0; noun <= 99; noun++) {
    for (let verb = 0; verb <= 99; verb++) {
      const buffer = [...code];
      buffer[1] = noun;
      buffer[2] = verb;

      const result = interpret(buffer)[0];
      if (result == target) {
        return {
          noun,
          verb,
        };
      }
    }
  }

  // Lazy failsafe, should never happen
  return {
    noun: 0,
    verb: 0,
  };
}

const target = 19690720;
const { noun, verb } = findMatchingWords(initialState, target);

const output2 = 100 * noun + verb;

export {
  output1 as solutionPartOne,
  output2 as solutionPartTwo,
};
