:- module(day07, []).
:- use_module(capellic(rcv), []).

:- dynamic map/1.
at(P,C) :- map(Map), rcv:peek_1b(Map,P,C).

solve(Kind, P1,P2) :-
    retractall(map(_)),
    abolish_all_tables,

    rcv:from_file(Kind,Map),
    assert(map(Map)),

    at(S,0'S),
    tachyon_beam_splits(S, P2),

    P1=0.

:- table tachyon_beam_splits(+,-).

tachyon_beam_splits(C, Splits) :-
    rcv:d_move(C,D),
    at(D,Ch),
    !,
    (   Ch == 0'.
    ->  tachyon_beam_splits(D, Splits)
    ;   Ch == 0'^
    ->  rcv:l_move(C,L),
        rcv:r_move(C,R),
        tachyon_beam_splits(L, Ln),
        tachyon_beam_splits(R, Lr),
        Splits is Ln+Lr
    ).
tachyon_beam_splits(_, 1).
