:- module(_, []).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(ugraphs)).

solve(Kind, P1,P2) :-
    phrase_from_file(sequence(node_edges,NodeEdges),Kind),!,
    part1(NodeEdges,P1),
    part2(NodeEdges,P2).

part1(NodeEdges,P1) :-
    (   setof(P,path(you,out,NodeEdges,[],P),Ps)
    ->  length(Ps,P1)
    ;   P1 = '#N/A'
    ).

% naive DFS solution, should work for general graphs
path(Curr,Target,NodeEdges,Seen,Path) :-
    (  Curr == Target
    -> Path = Seen,
       debug(day11,'~w',[[Curr|Path]])
    ;  member(Curr-Edges,NodeEdges),
       member(Next,Edges),
       \+ memberchk(Next,Seen),
       path(Next,Target,NodeEdges,[Curr|Seen],Path)
    ).

part2(NodeEdges,P2) :-
    check_dag(NodeEdges,UGraph,_TopoSorted),
    count_paths_(svr,fft,UGraph, FFT),
    count_paths_(fft,dac,UGraph, DAC),
    count_paths_(dac,out,UGraph, OUT),
    P2 is FFT*DAC*OUT.

count_paths_(S,T,U,N) :-
    % NB: without cleanup, doesn't work
    abolish_table_subgoals(count_paths(_,_,_,_)),
    count_paths(S,T,U,N).

:- table count_paths(+,+,+,-).
count_paths(T,T,_,1).
count_paths(S,T,U,N) :-
    memberchk(S-Cs,U),
    aggregate_all(sum(V), (
       member(C,Cs),
       count_paths(C,T,U,V)
    ),N).

% check DAGness (implies topologically sortable)
check_dag(NodeEdges,UGraph,TopSorted) :-
    findall(S-T, (
       member(S-Es,NodeEdges),
       member(T,Es)
    ), EdgesPairs),
    vertices_edges_to_ugraph([],EdgesPairs,UGraph),
    top_sort(UGraph,TopSorted).

% parsing

node_edges(Node-Edges) -->
    csym(Node), ": ", sequence(csym, " ", Edges), eol.

