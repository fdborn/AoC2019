type parameterMode =
  | Position
  | Immediate;

type instruction =
  | Add(parameterMode, parameterMode)
  | Multiply(parameterMode, parameterMode)
  | Input
  | Output(parameterMode)
  | Halt
  | Invalid;

type code = array(int);

type state = {
  memory: code,
  headPos: int,
};

type machine =
  | Running(state)
  | WaitingForInput(state, (code, int) => code)
  | HasOutput(state, int)
  | Halted(state);

let parseOpCode = instruction =>
  if (String.length(instruction) > 2) {
    int_of_string(
      String.sub(instruction, String.length(instruction) - 2, 2),
    );
  } else {
    int_of_string(instruction);
  };

let parseParameterMode = (instruction, index) => {
  switch (instruction.[String.length(instruction) - index - 3]) {
  | '1' => Immediate
  | _ => Position
  | exception (Invalid_argument(_)) => Position
  };
};

let parseInstruction = number => {
  let instruction = string_of_int(number);
  let opCode = parseOpCode(instruction);

  switch (opCode) {
  | 1 =>
    let lhsMode = parseParameterMode(instruction, 0);
    let rhsMode = parseParameterMode(instruction, 1);
    Add(lhsMode, rhsMode);
  | 2 =>
    let lhsMode = parseParameterMode(instruction, 0);
    let rhsMode = parseParameterMode(instruction, 1);
    Multiply(lhsMode, rhsMode);
  | 3 => Input
  | 4 =>
    let lhsMode = parseParameterMode(instruction, 0);
    Output(lhsMode);
  | 99 => Halt
  | _ => Invalid
  };
};

let readMemory = (memory, mode, pos) => {
  switch (mode) {
  | Position => memory[memory[pos]]
  | Immediate => memory[pos]
  };
};

let transitionState = ({memory, headPos}) => {
  let read = readMemory(memory);

  switch (parseInstruction(memory[headPos])) {
  | Add(lhsMode, rhsMode) =>
    let lhs = read(lhsMode, headPos + 1);
    let rhs = read(rhsMode, headPos + 2);
    let outAddr = memory[headPos + 3];
    memory[outAddr] = lhs + rhs;
    Running({memory, headPos: headPos + 4});
  | Multiply(lhsMode, rhsMode) =>
    let lhs = read(lhsMode, headPos + 1);
    let rhs = read(rhsMode, headPos + 2);
    let outAddr = memory[headPos + 3];
    memory[outAddr] = lhs * rhs;
    Running({memory, headPos: headPos + 4});
  | Input =>
    let inputPos = memory[headPos + 1];
    let updateFn = (memory, input) => {
      memory[inputPos] = input;
      memory;
    };
    WaitingForInput({memory, headPos: headPos + 2}, updateFn);
  | Output(mode) =>
    let output = read(mode, headPos + 1);
    HasOutput({memory, headPos: headPos + 2}, output);
  | Halt
  | Invalid => Halted({memory, headPos})
  };
};

type inputFunction = unit => int;
type outputFunction = int => unit;

let rec run =
        (state: state, ~input: inputFunction, ~output: outputFunction) => {
  let next = run(~input, ~output);

  switch (transitionState(state)) {
  | Running(state) => next(state)
  | WaitingForInput(state, applyUpdate) =>
    next({...state, memory: applyUpdate(state.memory, input())})
  | HasOutput(state, programOutput) =>
    output(programOutput);
    next(state);
  | Halted(state) => state
  };
};

let load = (code: code) => {
  memory: Array.copy(code),
  headPos: 0,
};
