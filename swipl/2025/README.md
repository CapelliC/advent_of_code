# advent_of_code 2025

solutions in SWI-Prolog to Advent of Code puzzles for year 2025

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

## day 10 (part 2) was interesting.

You can [read](day10/README.md), a brief comment about my multiple attemps, and notice
an apparently small change the PL sources: now I use the directive `:- module(_, []).`
instead of `:- module(day10, []).`, so the module name always matches the source, and saving with name some attemps
doesn't compromise the usability. What I mean: you can open - for instance - day10_fd_too_slow.pl, compile it and run the code,
just prefixing the solve/3 call: `> day10_fd_too_slow:solve(small,A,B).`.

