:- module(day08, []).

solve(Kind, P1,_P2) :-
    parse(Kind, _JunctionBoxesPts,NumShortestConnections,SquaredDistances),
    connect_jbs(NumShortestConnections,SquaredDistances,Circuits),
    maplist(size_nb_set, Circuits, Sizes),
    sort(Sizes,Sorted),
    append(_, [A,B,C], Sorted),
    P1 is A*B*C.

connect_jbs(NumShortestConnections,SquaredDistances, Circuits) :-
    rb_empty(RbTree),
    connect_jbs(NumShortestConnections,SquaredDistances, RbTree,[], Circuits).

connect_jbs(NumShortestConnections,SquaredDistances, RbTree,NbSets, Circuits) :-
    NumShortestConnections > 0,
    SquaredDistances = [_-(C1,C2)|SquaredDistancesRest],
    (   rb_lookup(C1, I1, RbTree)
    ->  true
    ;   I1 = 0
    ),
    (   rb_lookup(C2, I2, RbTree)
    ->  true
    ;   I2 = 0
    ),
    (   I1 = 0, I2 = 0
    ->  empty_nb_set(New),
        add_nb_set(C1,New),
        add_nb_set(C2,New),
        append(NbSets,[New],NbSetsU),
        length(NbSetsU,IdNew),
        rb_insert(RbTree,C1,IdNew,RbTreeT),
        rb_insert(RbTreeT,C2,IdNew,RbTreeU)
    ;   I1 > 0, I2 = 0
    ->  nth1(I1,NbSets,NbSet1),
        add_nb_set(C2,NbSet1),
        NbSetsU = NbSets,
        rb_insert(RbTree,C2,I1,RbTreeU)
    ;   I1 = 0, I2 > 0
    ->  nth1(I2,NbSets,NbSet2),
        add_nb_set(C1,NbSet2),
        NbSetsU = NbSets,
        rb_insert(RbTree,C1,I2,RbTreeU)
    ;   I1 = I2
    ->  NbSetsU = NbSets,
        RbTreeU = RbTree
    ;   debug(day08,'~w',what_about(C1-I1,C2-I2)),

        nth1(I1,NbSets,NbSet1),
        add_nb_set(C2,NbSet1),
        nth1(I2,NbSets,NbSet2),
        add_nb_set(C1,NbSet2),

        NbSetsU = NbSets,
        RbTreeU = RbTree
    ),

    (   debugging(day08)
    ->  maplist(size_nb_set, NbSetsU, Sizes),
        writeq(sizes(Sizes,C1-I1,C2-I2)),nl
    ;   true
    ),

    NumShortestConnections1 is NumShortestConnections - 1,
    !, connect_jbs(NumShortestConnections1,SquaredDistancesRest, RbTreeU,NbSetsU, Circuits).

connect_jbs(_0, _SquaredDistances,_RbTree,NbSets, Circuits) :-
    Circuits = NbSets.

parse(Kind, JunctionBoxesPts,NumShortestConnections,SquaredDistances) :-
    read_file_to_string(Kind,String,[]),
    string_lines(String,Lines),
    maplist([Line,Dict]>>( % Dict==_{0:_,x:X,y:Y,z:Z}
      re_matchsub('(?<x_I>\\d+),(?<y_I>\\d+),(?<z_I>\\d+)',Line,Dict)
    ),Lines,JunctionBoxesPts),
    (   Kind==small
    ->  NumShortestConnections=10000
    ;   NumShortestConnections=10000000
    ),
    setof(SqDist, squared_distances(JunctionBoxesPts,SqDist), SquaredDistances).

squared_distances(JBs,SqDist-(Ja,Jb)) :-
    nth1(I,JBs,_{0:Ja,x:Xa,y:Ya,z:Za}),
    nth1(J,JBs,_{0:Jb,x:Xb,y:Yb,z:Zb}),
    I\==J,
    SqDist is sqrt((Xa-Xb)*(Xa-Xb) + (Ya-Yb)*(Ya-Yb) + (Za-Zb)*(Za-Zb)).
