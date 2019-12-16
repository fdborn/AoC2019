open StdLabels;
module StringMap = Belt.Map.String;

[@bs.module "./input.txt"] external input: string = "default";

let orbits =
  Js.String.split("\n", input)
  |> Js.Array.filter(String.contains(_, ')'))
  |> Array.fold_left(~init=StringMap.empty, ~f=(orbits, line) =>
       switch (Js.String.split(")", line)) {
       | [|planet, satellite|] when StringMap.has(orbits, planet) =>
         StringMap.set(
           orbits,
           planet,
           [satellite, ...StringMap.getExn(orbits, planet)],
         )
       | [|planet, satellite|] => StringMap.set(orbits, planet, [satellite])
       | _ => orbits
       }
     );

module Tree = {
  type t =
    | Node(string, list(t));

  let rec init = (~children: string => list(string), start: string) => {
    let childNodes = List.map(~f=init(~children), children(start));
    Node(start, childNodes);
  };
};

let rec countDepth = (~depth=0, Node(_, children): Tree.t) => {
  children
  |> List.fold_left(~init=depth, ~f=(counter, tree) => {
       counter + countDepth(tree, ~depth=depth + 1)
     });
};

let totalOrbits =
  Tree.init("COM", ~children=StringMap.getWithDefault(orbits, _, []))
  |> countDepth;

let solutionPartOne = totalOrbits;
let solutionPartTwo = 0;
