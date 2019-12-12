[@bs.module "./input.txt"] external input: string = "default";
[@bs.scope "window"] [@bs.val]
external prompt: string => string = "prompt";

let baseCode =
  Js.String.replace("\n", "", input)
  |> Js.String.split(",")
  |> Array.map(int_of_string);

let inputFn = () => int_of_string(prompt("Awaiting input"));
let outputFn = output => Js.log("Computer output: " ++ string_of_int(output));

let state = Computer.interpret(baseCode, ~inputFn, ~outputFn);

let solutionPartOne = 0;
let solutionPartTwo = 0;
