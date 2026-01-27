# advent_of_code

Solutions in Dart to Advent of Code puzzles.

## Intro.
As stated in ../README.md, no puzzle text or data is uploaded into the repo, apart the first day snall example text.
So, after cloned the repository, just the first day is runnable, and only with the *small* data file.
Practically, in launch.json uncomment the *args* entry

```
  //"args": ["small"],
```
Now aoc_2025_01.dart should runnable, and you should get

```
p1:3 p2:6 in 26 ms
```

In aocSwiPath.dart you'll find usage of the environment variable *AOC_SWI_PATH*, that allows to relocate the folder holding the days data.
Of course, the easiest way would be to just place the *input* data file in the 'standard' path, i.e.

```
advent_of_code/swipl/2025/day01/input
advent_of_code/swipl/2025/day02/input
etc etc
```

## BUGS

Note: there is a bug in day 5, as p2 is over the correct answer by 12, then I'll try to debug matching the SWI-Prolog solution ASAP.

## Using Dart for debugging ease

Day 11 part 2 has been solved after verifying the kind of graph, using SWI-Prolog:

```
:- use_module(library(ugraphs)).

part2(NodeEdges,P2) :-

    % check if graph is DAG (that implies is topologically sortable)
    findall(S-T, (
       member(S-Es,NodeEdges),
       member(T,Es)
    ), EdgesPairs),
    vertices_edges_to_ugraph([],EdgesPairs,UGraph),
    top_sort(UGraph,Sorted),
    ...
```    

