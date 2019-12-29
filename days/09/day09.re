open StdLabels;
[@bs.scope "window"] [@bs.val] external prompt: string => string = "prompt";
[@bs.module "./input.txt"] external inputString: string = "default";

let input = () => float_of_string(prompt("Awaiting input"));
let output = output =>
  Js.log("Computer output: " ++ Js.Float.toString(output));

inputString
|> Js.String.replace("\n", "")
|> Js.String.split(",")
|> Array.map(~f=float_of_string)
|> FComputer.load
|> FComputer.addDevices(~input, ~output)
|> FComputer.run;

let solutionPartOne = 0;
let solutionPartTwo = 0;
