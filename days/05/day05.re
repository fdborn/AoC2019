[@bs.module "./input.txt"] external input: string = "default";

let baseCode =
  Js.String.replace("\n", "", input)
  |> Js.String.split(",")
  |> Array.map(int_of_string);

baseCode[1] = 12;
baseCode[2] = 2;

let solutionPartOne = Computer.runCode(baseCode)[0];

let solutionPartTwo = 0;
