# advent_of_code
solutions in SWI-Prolog to Advent of Code puzzles

## configuration

In some puzzles (days 4,6,7 sofar), I reuse a comfy 2d data structure, namely the module *rcv.pl*.
To execute such scripts you should add in your init.pl this snippet:
```
:- multifile user:file_search_path/2.
user:file_search_path(capellic, '- where you git cloned the repo -/advent_of_code/swipl') :-
```

## day 7 was funny.

I was struggling to get the small data solution of part 1, with tabling.
Then I quickly wrote part 1 in Dart, and once unlocked part 2, LOL, the answer for the small data was the same I got (wrongly) from tabled part 1.
So, tried with the large data, and got the puzzle solved !

## bugs

Day 8 doesn't work, after ten iteration (for the example data), I have just sets with 4 elements,
maybe I don't understand when to stop and what to do when two points fall already into 2 different sets

