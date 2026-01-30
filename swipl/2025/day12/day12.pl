:- module(_, []).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

part1(Kind,(S,A)) :-
    parse(Kind,S,A).

part2(Kind,_N) :-
    parse(Kind,_,_).

parse(Kind,Shapes,Aree) :-
    phrase_from_file((shapes(Shapes),aree(Aree)),Kind),
    length(Shapes,N),
    maplist({N}/[(_*_)-Ps]>>length(Ps,N),Aree).

shapes(Shapes) -->
    sequence(shape,Shapes).
shape([L1,L2,L3]) -->
    integer(_),":",eol,
    line(L1),
    line(L2),
    line(L3),eol.
line([A,B,C]) -->
    [A,B,C],eol.

aree(Aree) -->
    sequence(area,Aree).
area((X*Y)-Presents) -->
    integer(X),"x",integer(Y),": ",sequence(integer," ",Presents), eol.
