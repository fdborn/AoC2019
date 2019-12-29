open StdLabels;

type parameterMode =
  | Position
  | Immediate
  | Relative;

type instruction =
  | Add(parameterMode, parameterMode, parameterMode)
  | Multiply(parameterMode, parameterMode, parameterMode)
  | Input(parameterMode)
  | Output(parameterMode)
  | JumpIfTrue(parameterMode, parameterMode)
  | JumpIfFalse(parameterMode, parameterMode)
  | LessThan(parameterMode, parameterMode, parameterMode)
  | Equals(parameterMode, parameterMode, parameterMode)
  | SetRelative(parameterMode)
  | Halt
  | Invalid;

type memory = array(float);

type inputFunction = unit => float;
type outputFunction = float => unit;

type context = {
  mem: memory,
  headPos: int,
  input: inputFunction,
  output: outputFunction,
  relativeBase: int,
};

type state =
  | Running(context)
  | WaitingForInput(context, float => unit)
  | HasOutput(context, float)
  | Halted(context);

let parseOpCode = (instruction: string) =>
  if (String.length(instruction) > 2) {
    int_of_string(
      String.sub(instruction, ~pos=String.length(instruction) - 2, ~len=2),
    );
  } else {
    int_of_string(instruction);
  };

let parseParameterMode = (instruction: string, index: int) =>
  switch (instruction.[String.length(instruction) - index - 3]) {
  | '0' => Position
  | '1' => Immediate
  | '2' => Relative
  | _ => Position
  | exception (Invalid_argument(_)) => Position
  };

let parseInstruction = (number: float) => {
  let instruction = Js.Float.toString(number);
  let opCode = parseOpCode(instruction);

  switch (opCode) {
  | 1 =>
    let lhsMode = parseParameterMode(instruction, 0);
    let rhsMode = parseParameterMode(instruction, 1);
    let outMode = parseParameterMode(instruction, 2);
    Add(lhsMode, rhsMode, outMode);
  | 2 =>
    let lhsMode = parseParameterMode(instruction, 0);
    let rhsMode = parseParameterMode(instruction, 1);
    let outMode = parseParameterMode(instruction, 2);
    Multiply(lhsMode, rhsMode, outMode);
  | 3 =>
    let mode = parseParameterMode(instruction, 0);
    Input(mode);
  | 4 =>
    let mode = parseParameterMode(instruction, 0);
    Output(mode);
  | 5 =>
    let lhsMode = parseParameterMode(instruction, 0);
    let rhsMode = parseParameterMode(instruction, 1);
    JumpIfTrue(lhsMode, rhsMode);
  | 6 =>
    let lhsMode = parseParameterMode(instruction, 0);
    let rhsMode = parseParameterMode(instruction, 1);
    JumpIfFalse(lhsMode, rhsMode);
  | 7 =>
    let lhsMode = parseParameterMode(instruction, 0);
    let rhsMode = parseParameterMode(instruction, 1);
    let outMode = parseParameterMode(instruction, 2);
    LessThan(lhsMode, rhsMode, outMode);
  | 8 =>
    let lhsMode = parseParameterMode(instruction, 0);
    let rhsMode = parseParameterMode(instruction, 1);
    let outMode = parseParameterMode(instruction, 2);
    Equals(lhsMode, rhsMode, outMode);
  | 9 =>
    let mode = parseParameterMode(instruction, 0);
    SetRelative(mode);
  | 99 => Halt
  | _ => Invalid
  };
};

let readMemory =
    ({mem, relativeBase}: context, ~mode: parameterMode, ~pos: int) => {
  switch (mode) {
  | Position => mem[int_of_float(mem[pos])]
  | Immediate => mem[pos]
  | Relative => mem[relativeBase + int_of_float(mem[pos])]
  };
};

let writeMemory =
    (
      {mem, relativeBase}: context,
      ~mode: parameterMode,
      ~pos: int,
      ~input: float,
    ) =>
  switch (mode) {
  | Position => mem[int_of_float(mem[pos])] = input
  | Immediate => mem[pos] = input
  | Relative => mem[relativeBase + int_of_float(mem[pos])] = input
  };

let transition = (context: context) => {
  let read = readMemory(context);
  let write = writeMemory(context);
  let {headPos, relativeBase} = context;

  switch (parseInstruction(read(~mode=Immediate, ~pos=headPos))) {
  | Add(lhsMode, rhsMode, outMode) =>
    let lhs = read(~mode=lhsMode, ~pos=headPos + 1);
    let rhs = read(~mode=rhsMode, ~pos=headPos + 2);
    write(~mode=outMode, ~pos=headPos + 3, ~input=lhs +. rhs);
    Running({...context, headPos: headPos + 4});
  | Multiply(lhsMode, rhsMode, outMode) =>
    let lhs = read(~mode=lhsMode, ~pos=headPos + 1);
    let rhs = read(~mode=rhsMode, ~pos=headPos + 2);
    write(~mode=outMode, ~pos=headPos + 3, ~input=lhs *. rhs);
    Running({...context, headPos: headPos + 4});
  | Input(mode) =>
    let updateFn = (input: float) => {
      write(~mode, ~pos=headPos + 1, ~input);
    };
    WaitingForInput({...context, headPos: headPos + 2}, updateFn);
  | Output(mode) =>
    let output = read(~mode, ~pos=headPos + 1);
    HasOutput({...context, headPos: headPos + 2}, output);
  | JumpIfTrue(lhsMode, rhsMode) =>
    let conditional = read(~mode=lhsMode, ~pos=headPos + 1);
    let newPos = int_of_float(read(~mode=rhsMode, ~pos=headPos + 2));
    if (conditional != 0.0) {
      Running({...context, headPos: newPos});
    } else {
      Running({...context, headPos: headPos + 3});
    };
  | JumpIfFalse(lhsMode, rhsMode) =>
    let conditional = read(~mode=lhsMode, ~pos=headPos + 1);
    let newPos = int_of_float(read(~mode=rhsMode, ~pos=headPos + 2));
    if (conditional == 0.0) {
      Running({...context, headPos: newPos});
    } else {
      Running({...context, headPos: headPos + 3});
    };
  | LessThan(lhsMode, rhsMode, outMode) =>
    let lhs = read(~mode=lhsMode, ~pos=headPos + 1);
    let rhs = read(~mode=rhsMode, ~pos=headPos + 2);
    write(~mode=outMode, ~pos=headPos + 3, ~input=lhs < rhs ? 1.0 : 0.0);
    Running({...context, headPos: headPos + 4});
  | Equals(lhsMode, rhsMode, outMode) =>
    let lhs = read(~mode=lhsMode, ~pos=headPos + 1);
    let rhs = read(~mode=rhsMode, ~pos=headPos + 2);
    write(~mode=outMode, ~pos=headPos + 3, ~input=lhs == rhs ? 1.0 : 0.0);
    Running({...context, headPos: headPos + 4});
  | SetRelative(mode) =>
    let offset = int_of_float(read(~mode, ~pos=headPos + 1));
    Running({
      ...context,
      headPos: headPos + 2,
      relativeBase: relativeBase + offset,
    });
  | Halt
  | Invalid => Halted(context)
  };
};

let inputStub = () => 0.0;
let outputStub = _output => ();

let load = (~memorySize: int=64000, code: memory) => {
  let mem = Array.make(memorySize, 0.0);
  Array.blit(
    ~src=code,
    ~src_pos=0,
    ~dst=mem,
    ~dst_pos=0,
    ~len=Array.length(code),
  );

  {mem, headPos: 0, input: inputStub, output: outputStub, relativeBase: 0};
};

let addDevices =
    (context: context, ~input: inputFunction, ~output: outputFunction) => {
  ...context,
  input,
  output,
};

let rec run = (context: context) =>
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
