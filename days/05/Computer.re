type instruction =
  | Add
  | Multiply
  | Halt
  | Invalid;

type code = array(int);

let parseInstruction = number =>
  switch (number) {
  | 1 => Add
  | 2 => Multiply
  | 99 => Halt
  | _ => Invalid
  };

let runCode = (code): code => {
  let memory = Array.copy(code);
  let headPos = ref(0);

  while (headPos^ < Array.length(memory)) {
    switch (parseInstruction(memory[headPos^])) {
    | Add =>
      let lhs = memory[memory[headPos^ + 1]];
      let rhs = memory[memory[headPos^ + 2]];
      let outAddr = memory[headPos^ + 3];
      memory[outAddr] = lhs + rhs;
      headPos := headPos^ + 4;
    | Multiply =>
      let lhs = memory[memory[headPos^ + 1]];
      let rhs = memory[memory[headPos^ + 2]];
      let outAddr = memory[headPos^ + 3];
      memory[outAddr] = lhs * rhs;
      headPos := headPos^ + 4;
    | Halt
    | Invalid => headPos := Array.length(memory)
    };
  };

  memory;
};
