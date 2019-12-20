[@bs.module "./input.txt"] external inputString: string = "default";
[@bs.scope "window"] [@bs.val] external prompt: string => string = "prompt";

let input = () => int_of_string(prompt("Awaiting input"));
let output = output => Js.log("Computer output: " ++ string_of_int(output));

Js.String.replace("\n", "", inputString)
|> Js.String.split(",")
|> Array.map(int_of_string)
|> Computer.load
|> Computer.addDevices(~input, ~output)
|> Computer.run;

// No real solutions this time
let solutionPartOne = 0;

let solutionPartTwo = 0;
