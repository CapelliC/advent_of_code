:- module(day09, []).

solve(Kind, P1,P2) :-
    read_file_to_string(Kind,String,[]),
    re_foldl(add_tile,'(?<x_I>\\d+),(?<y_I>\\d+)',String,[],RevTiles,[]),
    reverse(RevTiles,Tiles),
    aggregate_all(max(A),area(Tiles,A),P1),
    P2=0.

area(Tiles,Area) :-
    nth1(I,Tiles,(X1,Y1)),
    nth1(J,Tiles,(X2,Y2)),
    I\==J,
    Area is abs(X2 - X1 + 1) * abs(Y2 - Y1 + 1),
    debug(day09,'~w',[Area,(X1,Y1),(X2,Y2)]).

add_tile(_{0:_,x:X,y:Y}, Tiles, [(X,Y)|Tiles]).
