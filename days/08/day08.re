open StdLabels;
[@bs.module "./input.txt"] external input: string = "default";

let segmentize = (segmentLength, thelist) => {
  thelist
  |> List.fold_left(~init=[], ~f=(acc, current) =>
       switch (acc) {
       | [] => [[current]]
       | [hd, ...tl] when List.length(hd) < segmentLength => [
           hd @ [current],
           ...tl,
         ]
       | _ => [[current], ...acc]
       }
     )
  |> List.rev;
};

let parseImageLayers = (~width, ~height, imageData) => {
  let imageDataLength = List.length(imageData);
  let numLayers = imageDataLength / (width * height);
  let layerSize = imageDataLength / numLayers;
  segmentize(layerSize, imageData);
};

let countDigits = (~digit, layer) =>
  List.fold_left(
    ~init=0,
    ~f=
      (acc, current) =>
        if (int_of_string(current) == digit) {
          acc + 1;
        } else {
          acc;
        },
    layer,
  );

// Part one

let imageLayers =
  input
  |> Js.String.replace("\n", "")
  |> Js.String.split("")
  |> Array.to_list
  |> parseImageLayers(~width=25, ~height=6);

let layerWithFewestZeroes =
  imageLayers
  |> List.fold_left(~init=[], ~f=(acc, layer) =>
       switch (acc) {
       | [] => layer
       | _ when countDigits(~digit=0, layer) < countDigits(~digit=0, acc) => layer
       | _ => acc
       }
     );

let solutionPartOne =
  countDigits(~digit=1, layerWithFewestZeroes)
  * countDigits(~digit=2, layerWithFewestZeroes);

let image =
  imageLayers
  |> List.fold_left(
       ~init=Array.make_matrix(~dimx=25, ~dimy=6, 2),
       ~f=(image, layer) => {
         layer
         |> List.iteri(~f=(index, pixel) => {
              let x = index mod 25;
              let y = index / 25;

              switch (image[x][y]) {
              | 2 => image[x][y] = int_of_string(pixel)
              | _ => ()
              };
            });
         image;
       },
     );

// Part two

let asciiImage = ref("");

for (y in 0 to 5) {
  for (x in 0 to 24) {
    switch (image[x][y]) {
    | 0 => asciiImage := asciiImage^ ++ "#"
    | 1 => asciiImage := asciiImage^ ++ " "
    | _ => ()
    };
  };

  asciiImage := asciiImage^ ++ "\n";
};

let solutionPartTwo = "\n" ++ asciiImage^;
