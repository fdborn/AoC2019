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

type memory = array(int);

type inputFunction = unit => int;
type outputFunction = int => unit;

type context = {
  mem: memory,
  headPos: int,
  input: inputFunction,
  output: outputFunction,
};

type state =
  | Running(context)
  | WaitingForInput(context, int => unit)
  | HasOutput(context, int)
  | Halted(context);


let parseOpCode = (instruction: string) =>
  if (String.length(instruction) > 2) {
    int_of_string(
      String.sub(instruction, String.length(instruction) - 2, 2),
    );
  } else {
    int_of_string(instruction);
  };

let parseParameterMode = (instruction: string, index: int) => {
  switch (instruction.[String.length(instruction) - index - 3]) {
  | '1' => Immediate
  | _ => Position
  | exception (Invalid_argument(_)) => Position
  };
};

let parseInstruction = (number: int) => {
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

let readMemory = ({mem}: context, ~mode: parameterMode, ~pos: int) => {
  switch (mode) {
  | Position => mem[mem[pos]]
  | Immediate => mem[pos]
  };
};

let writeMemory = ({mem}: context, ~pos: int, ~input: int) =>
  mem[pos] = input;

let transition = (context: context) => {
  let read = readMemory(context);
  let write = writeMemory(context);
  let {headPos} = context;

  switch (parseInstruction(read(~mode=Immediate, ~pos=context.headPos))) {
  | Add(lhsMode, rhsMode) =>
    let lhs = read(~mode=lhsMode, ~pos=headPos + 1);
    let rhs = read(~mode=rhsMode, ~pos=headPos + 2);
    let outAddr = read(~mode=Immediate, ~pos=headPos + 3);
    write(~pos=outAddr, ~input=lhs + rhs);
    Running({...context, headPos: headPos + 4});
  | Multiply(lhsMode, rhsMode) =>
    let lhs = read(~mode=lhsMode, ~pos=headPos + 1);
    let rhs = read(~mode=rhsMode, ~pos=headPos + 2);
    let outAddr = read(~mode=Immediate, ~pos=headPos + 3);
    write(~pos=outAddr, ~input=lhs * rhs);
    Running({...context, headPos: headPos + 4});
  | Input =>
    let inputPos = read(~mode=Immediate, ~pos=headPos + 1);
    let updateFn = (input: int) => {
      write(~pos=inputPos, ~input);
    };
    WaitingForInput({...context, headPos: headPos + 2}, updateFn);
  | Output(mode) =>
    let output = read(~mode, ~pos=headPos + 1);
    HasOutput({...context, headPos: headPos + 2}, output);
  | Halt
  | Invalid => Halted(context)
  };
};

let inputStub = () => 0;
let outputStub = _output => ();

let load = (code: memory) => {
  mem: Array.copy(code),
  headPos: 0,
  input: inputStub,
  output: outputStub,
};

let addDevices =
    (context: context, ~input: inputFunction, ~output: outputFunction) => {
  ...context,
  input,
  output,

};

let rec run = (context: context) => {
  switch (transition(context)) {
  | Running(context) => run(context)
  | WaitingForInput(context, applyUpdate) =>
    applyUpdate(context.input());
    run(context);
  | HasOutput(context, programOutput) =>
    context.output(programOutput);
    run(context);
  | Halted(context) => context
  };
};
