const solutions = [
  import('./days/01/'),
  import('./days/02/'),
  import('./days/03/'),
  import('./days/04/day04.re'),
  import('./days/05/day05.re'),
  import('./days/06/day06.re'),
  import('./days/07/day07.re'),
  import('./days/08/day08.re'),
  import('./days/09/day09.re'),
  import('./days/10/day10.re'),
];

document.addEventListener('DOMContentLoaded', () => {
  const buttons = document.querySelectorAll('.show-solution');

  buttons.forEach(button => button.addEventListener('click', e => {
    const day = parseInt((e.target as HTMLButtonElement).dataset.day!);

    console.log(`Solutions for Day ${day}:`);

    solutions[day - 1].then(({solutionPartOne, solutionPartTwo}) => {
      Promise.resolve(solutionPartOne)
        .then(solution => console.log(`Part one: ${solution}`));

      Promise.resolve(solutionPartTwo)
        .then(solution => console.log(`Part two: ${solution}`));
    });
  }));
});

export {}
