open StdLabels;
open MoreLabels;

module Moon = {
  type t = {
    position: (int, int, int),
    mutable velocity: (int, int, int),
  };

  let make = (x, y, z) => {position: (x, y, z), velocity: (0, 0, 0)};

  let updateVelocity = (~x=0, ~y=0, ~z=0, moon) => {
    let (velX, velY, velZ) = moon.velocity;
    moon.velocity = (velX + x, velY + y, velZ + z);
  };

  let applyGravity = ((left: t, right: t)) => {
    let (leftX, leftY, leftZ) = left.position;
    let (rightX, rightY, rightZ) = right.position;

    if (leftX < rightX) {
      updateVelocity(~x=1, left);
      updateVelocity(~x=-1, right);
    } else if (leftX > rightX) {
      updateVelocity(~x=-1, left);
      updateVelocity(~x=1, right);
    };

    if (leftY < rightY) {
      updateVelocity(~y=1, left);
      updateVelocity(~y=-1, right);
    } else if (leftY > rightY) {
      updateVelocity(~y=-1, left);
      updateVelocity(~y=1, right);
    };

    if (leftZ < rightZ) {
      updateVelocity(~z=1, left);
      updateVelocity(~z=-1, right);
    } else if (leftZ > rightZ) {
      updateVelocity(~z=-1, left);
      updateVelocity(~z=1, right);
    };
  };

  let applyVelocity = (moon: t) => {
    let (x, y, z) = moon.position;
    let (velX, velY, velZ) = moon.velocity;

    {...moon, position: (x + velX, y + velY, z + velZ)};
  };

  let getEnergy = (moon: t) => {
    let (x, y, z) = moon.position;
    let (velX, velY, velZ) = moon.velocity;
    let abs = Js.Math.abs_int;
    (abs(x) + abs(y) + abs(z)) * (abs(velX) + abs(velY) + abs(velZ));
  };
};

let rec pairs = (input: list('a)) =>
  switch (input) {
  | [] => []
  | [hd, ...tl] => List.map(~f=other => (hd, other), tl) @ pairs(tl)
  };

let simulate = (~steps, moons) => {
  let rec step = (moons, i) =>
    if (i < steps) {
      // Update velocity values
      List.iter(~f=Moon.applyGravity, pairs(moons));

      // Apply new velocity values
      let moons = List.map(~f=Moon.applyVelocity, moons);

      step(moons, i + 1);
    } else {
      moons;
    };

  step(moons, 0);
};

// Input
let io = Moon.make(1, 4, 4);
let europa = Moon.make(-4, -1, 19);
let ganymede = Moon.make(-15, -14, 12);
let callisto = Moon.make(-17, 1, 10);

let moons = [io, europa, ganymede, callisto];

let solutionPartOne =
  simulate(~steps=1000, moons)
  |> List.map(~f=Moon.getEnergy)
  |> List.fold_left(~init=0, ~f=(+));

let solutionPartTwo = 0;
