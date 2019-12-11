let isLegalPassword = password => {
  let isIncremental = ref(true);
  let hasDouble = ref(false);

  for (index in 1 to String.length(password) - 1) {
    let last = String.get(password, index - 1);
    let current = String.get(password, index);

    if (!hasDouble^ && last == current) {
      hasDouble := true;
    };

    if (isIncremental^) {
      isIncremental := last <= current;
    };
  };

  isIncremental^ && hasDouble^;
};

let hasOneDouble = password => {
  let hasDouble = ref(false);
  let blacklist = ref([]);

  for (index in 1 to String.length(password) - 1) {
    let last = String.get(password, index - 1);
    let current = String.get(password, index);

    let isNotInBlacklist = character => !List.exists((==)(character), blacklist^);

    if (!hasDouble^) {
      if (last == current && isNotInBlacklist(current)) {
        if (index < String.length(password) - 1) {
          let next = String.get(password, index + 1);
          if (current == next) {
            blacklist := [current, ...blacklist^];
          } else {
            hasDouble := true;
          };
        } else {
          hasDouble := true;
        };
      };
    };
  };

  hasDouble^;
};

let inputLower = 271973;
let inputUpper = 785961;

let legalPasswords = Array.init(inputUpper - inputLower + 1, i => inputLower + i)
|> Js.Array.map(string_of_int)
|> Js.Array.filter(isLegalPassword);

let solutionPartOne = Js.Array.length(legalPasswords);

let partTwoLegalPasswords = legalPasswords
  |> Js.Array.filter(hasOneDouble);

let solutionPartTwo = Js.Array.length(partTwoLegalPasswords);
