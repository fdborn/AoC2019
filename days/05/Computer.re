type instruction =
  | Add
  | Multiply
  | Input
  | Output
  | Halt
  | Invalid;

type code = array(int);

type state = {
  memory: code,
  headPos: int,
};

type machine =
  | Running(state)
  | InputWaiting(state, (code, int) => code)
  | OutputWaiting(state, int)
  | Halted(state);

let parseInstruction = number =>
  switch (number) {
  | 1 => Add
  | 2 => Multiply
  | 3 => Input
  | 4 => Output
  | 99 => Halt
  | _ => Invalid
  };

let transitionState = ({memory, headPos}) =>
  switch (parseInstruction(memory[headPos])) {
  | Add =>
    let lhs = memory[memory[headPos + 1]];
    let rhs = memory[memory[headPos + 2]];
    let outAddr = memory[headPos + 3];
    memory[outAddr] = lhs + rhs;
    Running({memory, headPos: headPos + 4});
  | Multiply =>
    let lhs = memory[memory[headPos + 1]];
    let rhs = memory[memory[headPos + 2]];
    let outAddr = memory[headPos + 3];
    memory[outAddr] = lhs * rhs;
    Running({memory, headPos: headPos + 4});
  | Input =>
    let inputPos = memory[headPos + 1];
    let updateFn = (memory, input) => {
      memory[inputPos] = input;
      memory;
    };
    InputWaiting({memory, headPos: headPos + 2}, updateFn);
  | Output =>
    let output = memory[memory[headPos + 1]];
    OutputWaiting({memory, headPos: headPos + 2}, output);
  | Halt
  | Invalid => Halted({memory, headPos})
  };

let interpret = (code, ~inputFn, ~outputFn) => {
  let machineState = ref({memory: Array.copy(code), headPos: 0});
  let isRunning = ref(true);

  while (isRunning^) {
    switch (transitionState(machineState^)) {
    | Running(state) => machineState := state
    | InputWaiting({memory, headPos}, updateFn) =>
      let input = inputFn();
      machineState := {memory: updateFn(memory, input), headPos};
    | OutputWaiting(state, output) =>
      outputFn(output);
      machineState := state;
    | Halted(state) =>
      isRunning := false;
      machineState := state;
    };
  };

  machineState^.memory;
};
