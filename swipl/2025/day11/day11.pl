:- module(_, []).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

solve(Kind, P1,P2) :-
    phrase_from_file(sequence(node_edges,NodeEdges),Kind),!,
    setof(P,path(you,out,NodeEdges,[],P),Ps),
    length(Ps,P1),
    P2 = 0.

path(Curr,Target,NodeEdges,Seen,Path) :-
    (  Curr == Target
    -> Path = Seen
    ;  member(Curr-Edges,NodeEdges),
       member(Next,Edges),
       \+ memberchk(Next,Seen),
       path(Next,Target,NodeEdges,[Curr|Seen],Path)
    ).

node_edges(Node-Edges) -->
    csym(Node), ": ", sequence(csym, " ", Edges), eol.
