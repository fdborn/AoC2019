open StdLabels;
open MoreLabels;

[@bs.module "./input.txt"] external input: string = "default";
module Computer = Day11Computer;

module PanelArea = {
  module Direction = {
    type t =
      | Up
      | Right
      | Down
      | Left;

    let turnRight = (direction: t) =>
      switch (direction) {
      | Up => Right
      | Right => Down
      | Down => Left
      | Left => Up
      };

    let turnLeft = (direction: t) =>
      switch (direction) {
      | Up => Left
      | Right => Up
      | Down => Right
      | Left => Down
      };
  };

  module Index = {
    type t = (int, int);

    let compare = ((x1, y1): t, (x2, y2): t) =>
      switch (Pervasives.compare(x1, x2)) {
      | 0 => Pervasives.compare(y1, y2)
      | n => n
      };

    let move = (~steps=1, ~direction: Direction.t, (x, y): t) =>
      switch (direction) {
      | Up => (x, y + steps)
      | Right => (x + steps, y)
      | Down => (x, y - steps)
      | Left => (x - steps, y)
      };
  };

  module Area = Map.Make(Index);

  type color =
    | Black
    | White;

  type t = ref(Area.t(color));

  let make = () => ref(Area.empty);

  let read = (~index: Index.t, panelArea: t) =>
    switch (Area.find_opt(index, panelArea^)) {
    | None => Black
    | Some(color) => color
    };

  let write = (~index: Index.t, ~color: color, panelArea: t) =>
    panelArea := Area.add(~key=index, ~data=color, panelArea^);
};

let runPaintProgram = (~panels=PanelArea.make(), code: array(float)) => {
  let position = ref((0, 0));
  let direction = ref(PanelArea.Direction.Up);

  let inputFn = () =>
    switch (PanelArea.read(~index=position^, panels)) {
    | Black => 0.
    | White => 1.
    };

  let makeOutputFn = () => {
    let colorBuffer = ref(None);

    output =>
      switch (colorBuffer^) {
      | None => colorBuffer := Some(output)
      | Some(color) =>
        let color =
          if (color == 1.) {
            PanelArea.White;
          } else {
            PanelArea.Black;
          };

        direction :=
          (
            if (output == 1.) {
              PanelArea.Direction.turnRight(direction^);
            } else {
              PanelArea.Direction.turnLeft(direction^);
            }
          );

        PanelArea.write(~index=position^, ~color, panels);
        position := PanelArea.Index.move(~direction=direction^, position^);

        colorBuffer := None;
      };
  };

  let _ =
    code
    |> Computer.load
    |> Computer.addDevices(~input=inputFn, ~output=makeOutputFn())
    |> Computer.run;

  panels^;
};

// Part one

let partOnePanels =
  input
  |> Js.String.replace("\n", "")
  |> Js.String.split(",")
  |> Array.map(~f=float_of_string)
  |> runPaintProgram;

let solutionPartOne = PanelArea.Area.cardinal(partOnePanels);

// Part two

let partTwoPanels = PanelArea.make();
PanelArea.write(~index=(0, 0), ~color=PanelArea.White, partTwoPanels);

let partTwoPanels =
  input
  |> Js.String.replace("\n", "")
  |> Js.String.split(",")
  |> Array.map(~f=float_of_string)
  |> runPaintProgram(~panels=partTwoPanels);

let (lowestX, lowestY) =
  partTwoPanels
  |> PanelArea.Area.fold(
       ~init=(0, 0), ~f=(~key as (x, y), ~data as _, (minX, minY)) =>
       (Js.Math.min_int(x, minX), Js.Math.min_int(y, minY))
     );

let (highestX, highestY) =
  partTwoPanels
  |> PanelArea.Area.fold(
       ~init=(0, 0), ~f=(~key as (x, y), ~data as _, (maxX, maxY)) =>
       (Js.Math.max_int(x, maxX), Js.Math.max_int(y, maxY))
     );

let paintImage =
    (~width: int, ~height: int, image: array(array(PanelArea.color))) => {
  let buffer = ref("");

  for (y in 0 to height - 1) {
    for (x in 0 to width - 1) {
      switch (image[x][height - y - 1]) {
      | PanelArea.Black => buffer := buffer^ ++ " "
      | PanelArea.White => buffer := buffer^ ++ "#"
      };
    };

    buffer := buffer^ ++ "\n";
  };

  buffer^;
};

let imageWidth = highestX - lowestX - (lowestX - lowestX) + 1;
let imageHeight = highestY - lowestY - (lowestY - lowestY) + 1;

let image =
  Array.make_matrix(~dimx=imageWidth, ~dimy=imageHeight, PanelArea.Black);

image[0 - lowestX][0 - lowestY] = PanelArea.White;

partTwoPanels
|> PanelArea.Area.iter(~f=(~key as (x, y), ~data) =>
     image[x - lowestX][y - lowestY] = data
   );

Js.log(paintImage(~width=imageWidth, ~height=imageHeight, image));
