open StdLabels;

[@bs.module "./input.txt"] external input: string = "default";

let code =
  input
  |> Js.String.replace("\n", "")
  |> Js.String.split(",")
  |> Array.map(~f=int_of_string);

let makeIterator = ((first, second): (int, int)) => {
  let repetitions = ref(0);

  () => {
    repetitions := repetitions^ + 1;
    repetitions^ == 1 ? first : second;
  };
};

module Amplifier = {
  type t = Repromise.t(int);

  let make = (~code: Computer.memory, ~phase: int, input) => {
    let (promise, resolvePromise) = Repromise.make();

    Computer.load(code)
    |> Computer.addDevices(
         ~input=makeIterator((phase, input)),
         ~output=resolvePromise,
       )
    |> Computer.run
    |> ignore;

    promise;
  };
};

let runChain = (~code: Computer.memory, phases: list(int)) =>
  List.fold_left(
    ~init=Repromise.resolved(0),
    ~f=
      (previous, phase) =>
        previous |> Repromise.andThen(Amplifier.make(~code, ~phase)),
    phases,
  );

// Functions to generate permutations
// Most parts stolen from http://typeocaml.com/2015/05/05/permutation/

let remove = (value, inputList) => List.filter(~f=(!=)(value), inputList);

let rec permutations = values =>
  switch (values) {
  | [] => []
  | [x] => [[x]]
  | values =>
    values
    |> List.fold_left(~init=[], ~f=(acc, current) => {
         acc
         @ List.map(
             ~f=p => [current, ...p],
             permutations(remove(current, values)),
           )
       })
  };

let phaseSettingPermutations = permutations([0, 1, 2, 3, 4]);
let maxSignal =
  phaseSettingPermutations
  |> List.fold_left(~init=Repromise.resolved(0), ~f=(max, permutation) => {
       max
       |> Repromise.andThen(maxValue => {
            runChain(~code, permutation)
            |> Repromise.map(currentValue =>
                 currentValue > maxValue ? currentValue : maxValue
               )
          })
     });

let solutionPartOne = maxSignal;

let solutionPartTwo = 0;
