[@bs.module "./input.txt"] external input: string = "default";
[@bs.scope "window"] [@bs.val]
external prompt: (string, string) => string = "prompt";

let baseCode =
  Js.String.replace("\n", "", "3,0,4,0,99")
  |> Js.String.split(",")
  |> Array.map(int_of_string);

let inputFn = () => int_of_string(prompt("Awaiting input", "0"));
let outputFn = Js.log;

let state = Computer.interpret(baseCode, ~inputFn=inputFn, ~outputFn=outputFn);

let solutionPartOne = 0;
let solutionPartTwo = 0;
