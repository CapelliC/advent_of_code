:- module(day03, []).

solve(Kind,P1,P2) :-
    read_file_to_string(Kind,File,[]),
    string_lines(File,Lines),
    foldl(day03line,Lines,(0,0,0),(_,P1,P2)).

day03line(Line,(I,P1,P2),(J,Q1,Q2)) :-
    string_codes(Line,Codes),
    accum_max_of_len(2,Codes,P1,Q1),
    accum_max_of_len(12,Codes,P2,Q2),
    J is I+1. % ease debugging

accum_max_of_len(N,Codes,P,Q) :-
    subset_n_maximal(N,Codes,Js),
    number_codes(M,Js),
    Q is P+M.

subset_n_maximal(0,_,[]) :- !.
subset_n_maximal(N,Cs,[Max|Rest]) :-
    SpareLen is N-1,
    length(Spare,SpareLen),
    append(ToScan,Spare,Cs),
    find_first_max_and_tail(ToScan, Max,Tail),
    append(Tail,Spare,NCs),
    !, subset_n_maximal(SpareLen,NCs,Rest).

find_first_max_and_tail([H|Cs], Max,Tail) :-
    find_max_and_tail(Cs,H,Cs, Max,Tail).

find_max_and_tail([C|Cs],CurrMax,CurrTail, Max,Tail) :-
    (   C > CurrMax
    ->  find_max_and_tail(Cs, C,Cs, Max,Tail)
    ;   find_max_and_tail(Cs, CurrMax,CurrTail, Max,Tail)
    ->  true
    ;   Max=CurrMax,
        Tail=CurrTail
    ),
    !.
find_max_and_tail([],CurrMax,CurrTail,CurrMax,CurrTail).

/*
solve(Kind,P1,P2) :-
    read_file_to_string(Kind,File,[]),
    string_lines(File,Lines),
    foldl(day03line,Lines,(0,0),(P1,P2)).

day03line(Line,(P1,P2),(Q1,Q2)) :-
    string_codes(Line,Codes),
    length(L1,2), aggregate_all(max(J), (peek(L1,Codes,Js), number_codes(J,Js)), J1),
    Q1 is P1+J1,
    length(L2,12), aggregate_all(max(J), (peek(L2,Codes,Js), number_codes(J,Js)), J2),
    Q2 is P2+J2.

peek([],_,[]) :- !.
peek([_|N],Cs,[C|Js]) :-
    append(_,[C|Rs],Cs),
    peek(N,Rs,Js).
*/

/*
day03line(Line,(P1,P2),(Q1,Q2)) :-
    string_codes(Line,Codes),
    aggregate_all(max(J), (peek(2,Codes,Js), number_codes(J,Js)), J1),
    Q1 is P1+J1,
    aggregate_all(max(J), (peek(12,Codes,Js), number_codes(J,Js)), J2),
    Q2 is P2+J2.

peek(0,_,[]) :- !.
peek(N,Cs,[C|Js]) :-
    append(_,[C|Rs],Cs),
    M is N-1,
    peek(M,Rs,Js).
*/
