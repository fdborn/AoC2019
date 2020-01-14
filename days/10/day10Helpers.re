open StdLabels;
open MoreLabels;

module UVector = {
  type t = (float, float);

  let make = (x, y): t => {
    let x = x;
    let y = y;
    let mag = Js.Math.sqrt(x ** 2. +. y ** 2.);

    // High performance float arithmetics
    let normalizedX =
      x
      /. mag
      |> Js.Float.toExponentialWithPrecision(_, ~digits=10)
      |> float_of_string;

    let normalizedY =
      y
      /. mag
      |> Js.Float.toExponentialWithPrecision(_, ~digits=10)
      |> float_of_string;

    (normalizedX, normalizedY);
  };

  let compare = ((x1, y1): t, (x2, y2): t) =>
    switch (Pervasives.compare(x1, x2)) {
    | 0 => Pervasives.compare(y1, y2)
    | n => n
    };

  let getRotation = ((x, y): t) => {
    // Get angle to x-axis
    let rad = ref(Js.Math.atan2(~x, ~y, ()));

    // Rotate the whole thing by 90°
    rad := rad^ -. Js.Math._PI /. 2.;

    // Change range from [-PI - PI] to [0 - 2PI]
    if (rad^ < 0.) {
      rad := rad^ +. Js.Math._PI *. 2.;
    };

    // Flip orientation to clockwise
    rad := Js.Math._PI *. 2. -. rad^;

    // Change 360° to 0°
    if (rad^ == Js.Math._PI *. 2.) {
      rad := 0.;
    };

    // To degree
    rad^ *. 180. /. Js.Math._PI;
  };
};

module UVectorMap = Map.Make(UVector);

module Index = {
  type t = (int, int);

  let make = (x, y) => (x, y);

  let compare = ((x1, y1), (x2, y2)) =>
    switch (Pervasives.compare(x1, x2)) {
    | 0 => Pervasives.compare(y1, y2)
    | n => n
    };
};

module Coordinate = {
  type t = (float, float);

  let make = (x, y) => (float_of_int(x), float_of_int(y) *. (-1.));

  let compare = ((x1, y1), (x2, y2)) =>
    switch (Pervasives.compare(x1, x2)) {
    | 0 => Pervasives.compare(y1, y2)
    | n => n
    };

  let getDistance = ((x1, y1), (x2, y2)) => {
    let vectorX = x2 -. x1;
    let vectorY = y2 -. y1;
    Js.Math.sqrt(vectorX ** 2. +. vectorY ** 2.);
  };
};

type position = (Index.t, Coordinate.t);

module Asteroid = {
  type t = {
    pos: position,
    visible: UVectorMap.t(array(position)),
  };

  let make = pos => {pos, visible: UVectorMap.empty};
};

module IndexMap = Map.Make(Index);
