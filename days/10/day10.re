open StdLabels;
open MoreLabels;
open Day10Helpers;

[@bs.module "./input.txt"] external input: string = "default";

let findVisibleVectors =
    (all: list(position), (originIndex, originCoords): position) => {
  let (originX, originY) = originCoords;

  all
  |> List.fold_left(
       ~init=UVectorMap.empty,
       ~f=(acc, position) => {
         let (index, coords) = position;
         let (x, y) = coords;

         if (index == originIndex) {
           acc;
         } else {
           let vectorX = x -. originX;
           let vectorY = y -. originY;

           let vector = UVector.make(vectorX, vectorY);

           switch (UVectorMap.find_opt(vector, acc)) {
           | Some(positions) =>
             UVectorMap.add(~key=vector, ~data=[position, ...positions], acc)
           | None => UVectorMap.add(~key=vector, ~data=[position], acc)
           };
         };
       },
     )
  |> UVectorMap.map(~f=positions => {
       positions
       |> List.sort(~cmp=(left, right) => {
            let (_, leftCoords) = left;
            let (_, rightCoords) = right;
            Pervasives.compare(
              Coordinate.getDistance(originCoords, leftCoords),
              Coordinate.getDistance(originCoords, rightCoords),
            );
          })
       |> List.rev
       |> Array.of_list
     });
};

// Part one

let positions =
  input
  |> Js.String.split("\n")
  |> Array.mapi(~f=(yCoordinate, row) =>
       row
       |> Js.String.split("")
       |> Belt.Array.reduceWithIndex(_, [], (acc, symbol, xCoordinate) =>
            switch (symbol) {
            | "#" =>
              [
                (
                  Index.make(xCoordinate, yCoordinate),
                  Coordinate.make(xCoordinate, yCoordinate),
                ),
              ]
              @ acc
            | _ => acc
            }
          )
     )
  |> Array.fold_left(~init=[], ~f=List.append);

let asteroids =
  positions
  |> List.fold_left(~init=IndexMap.empty, ~f=(acc, (index, _) as position) =>
       IndexMap.add(~key=index, ~data=Asteroid.make(position), acc)
     );

let asteroidsWithVectors =
  asteroids
  |> IndexMap.map(~f=(asteroid: Asteroid.t) =>
       Asteroid.{
         ...asteroid,
         visible: findVisibleVectors(positions, asteroid.pos),
       }
     );

let bestAsteroid =
  asteroidsWithVectors
  |> IndexMap.fold(~init=None, ~f=(~key as _, ~data: Asteroid.t, acc) => {
       switch (acc) {
       | None => Some(data)
       | Some((asteroid: Asteroid.t)) =>
         if (UVectorMap.cardinal(asteroid.visible)
             > UVectorMap.cardinal(data.visible)) {
           Some(asteroid);
         } else {
           Some(data);
         }
       }
     });

let solutionPartOne =
  switch (bestAsteroid) {
  | None => 0
  | Some(asteroid) => UVectorMap.cardinal(asteroid.visible)
  };

// Part two

let fireLaser = (vector, asteroid: Asteroid.t) =>
  switch (UVectorMap.find_opt(vector, asteroid.visible)) {
  | Some(targets) => Js.Array.pop(targets)
  | None => None
  };

let startLaser = (~from: Asteroid.t, numAsteroids) => {
  let angles =
    from.visible
    |> UVectorMap.bindings
    |> List.map(~f=((vector, _)) => vector)
    |> List.sort(~cmp=(left, right) =>
         Pervasives.compare(
           UVector.getRotation(left),
           UVector.getRotation(right),
         )
       );

  let getAngle = iteration =>
    List.nth(angles, iteration mod List.length(angles));

  let rec aux = (last, iteration, acc) =>
    switch (acc) {
    | asteroidsShot when asteroidsShot == numAsteroids => last
    | asteroidsShot =>
      switch (fireLaser(getAngle(iteration), from)) {
      | Some(_) as target => aux(target, iteration + 1, asteroidsShot + 1)
      | None => aux(None, iteration + 1, asteroidsShot)
      }
    };

  aux(None, 0, 0);
};

let solutionPartTwo =
  switch (bestAsteroid) {
  | Some(asteroid) =>
    switch (startLaser(~from=asteroid, 200)) {
    | Some(position) =>
      let (index, _) = position;
      let (x, y) = index;
      x * 100 + y;
    | None => 0
    }
  | None => 0
  };
