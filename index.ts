const solutions = [
  import('./days/01/'),
  import('./days/02/'),
  import('./days/03/'),
  import('./days/04/day04.re'),
];

document.addEventListener('DOMContentLoaded', () => {
  const buttons = document.querySelectorAll('.show-solution');

  buttons.forEach(button => button.addEventListener('click', e => {
    const day = parseInt((e.target as HTMLButtonElement).dataset.day!);

    console.log(`Solutions for Day ${day}:`);

    solutions[day - 1].then(({solutionPartOne, solutionPartTwo}) => {
      console.log(`Part one: ${solutionPartOne}`);
      console.log(`Part two: ${solutionPartTwo}`);
    });
  }));
});

export {}
