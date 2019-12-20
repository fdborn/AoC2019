open StdLabels;
module StringMap = Belt.Map.String;

[@bs.module "./input.txt"] external input: string = "default";

module Tree = {
  type t =
    | Node(string, list(t));

  let rec init = (~children: string => list(string), start: string) => {
    let childNodes = List.map(~f=init(~children), children(start));
    Node(start, childNodes);
  };

  let rec countDepth = (~depth=0, Node(_, children): t) => {
    children
    |> List.fold_left(~init=depth, ~f=(counter, tree) => {
         counter + countDepth(~depth=depth + 1, tree)
       });
  };

  let rec findCommonAncestor = (~search: (string, string), tree: t) => {
    let Node(name, children) = tree;
    let (searchLhs, searchRhs) = search;

    let prevMatch =
      children
      |> List.fold_left(~init=None, ~f=(acc: option(t), current: t) =>
           switch (acc, findCommonAncestor(~search, current)) {
           | (None, None) => None
           | (None, Some(node)) => Some(node)
           | (Some(node), None) => Some(node)
           | (Some(_), Some(_)) => Some(tree)
           }
         );

    let isMatch = searchLhs == name || searchRhs == name;
    switch (prevMatch, isMatch) {
    | (None, false) => None
    | (Some(node), false) => Some(node)
    | (_, true) => Some(tree)
    };
  };

  let rec countNodesTo = (~search: string, Node(name, children): t) => {
    let prevCount =
      children
      |> List.fold_left(~init=0, ~f=(acc: int, current: t) => {
           acc + countNodesTo(~search, current)
         });

    if (prevCount > 0) {
      prevCount + 1;
    } else {
      name == search ? 1 : 0;
    };
  };
};

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

// Part one

let orbitTree =
  Tree.init("COM", ~children=StringMap.getWithDefault(orbits, _, []));

let solutionPartOne = Tree.countDepth(orbitTree);

// Part two

let subTree =
  orbitTree
  |> Tree.findCommonAncestor(~search=("YOU", "SAN"))
  |> Belt.Option.getWithDefault(_, Tree.Node("stub tree", []));

// Distance is the total amount of nodes without the inner- and outermost node
let youDistance = Tree.countNodesTo(~search="YOU", subTree) - 2;
let sanDistance = Tree.countNodesTo(~search="SAN", subTree) - 2;

let solutionPartTwo = youDistance + sanDistance;
