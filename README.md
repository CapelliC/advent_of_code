# advent_of_code
solutions in SWI-Prolog, Dart, Picat to Advent of Code puzzles

## structure of this repo

I'm used to save the example and actual puzzle input data in the swipl year/day folder, along the source, naming them *small* (usually there is only 1 example), and *input*.

Then in swipl, I invoke switch into the folder of day of interest, consult the source, named the same as the folder, and then run ?- day01:solve(small,P1,P2).

Generally, as stated by the last FAQ you find [here](https://adventofcode.com/2025/about), puzzle text or inputs are not uploaded, and solutions are amended from actual computed values.

Just as comtrived example, to show the general pattern, I uploaded just the first day of 2025 example: *swipl/2025/day01/small*.

Then in swipl you can:

```
101 ?- cd('- where you git cloned the repo -/advent_of_code/swipl/2025/day01').
true.

102 ?- [day01].
true.

103 ?- day01:solve(small,P1,P2).
P1 = 3,
P2 = 6.
```
