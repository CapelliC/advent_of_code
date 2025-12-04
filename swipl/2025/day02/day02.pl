:- module(day02, []).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

solve(Kind,N1,N2) :-
    phrase_from_file(sequence(pair_ids,",",PairIDs),Kind),
    check_ids(invalid_p1,PairIDs,N1),
    check_ids(invalid_p2,PairIDs,N2).

:- meta_predicate check_ids(2,+,-).
check_ids(Check,PairIDs,T) :-
    aggregate_all(sum(N),(
      member(Id1-Id2,PairIDs),
      aggregate_all(sum(Inv),(
        between(Id1,Id2,Id),
        call(Check,Id,Inv)
      ),N)
    ),T).

invalid_p1(Id,Inv) :-
    number_string(Id,Ns),
    string_length(Ns,L2),
    0 is L2 mod 2,
    L is L2 // 2,
    sub_string(Ns,0,L,L,S),
    sub_string(Ns,L,L,0,S)
    -> Inv=Id ; Inv=0 .

invalid_p2(Id,Inv) :-
    Id > 9, % skip one digit numbers
    number_codes(Id,Ns),
    length(Ns,Ls),
    L2 is Ls // 2 + Ls mod 2,
    between(1,L2,L),
    length(P,L),
    append(P,R,Ns),
    invalid_p2_seq(R,P)
    -> Inv=Id ; Inv=0 .

invalid_p2_seq([],_).
invalid_p2_seq(S,P) :-
    append(P,R,S),
    !, invalid_p2_seq(R,P).
/* slower
invalid_p2_seq(S,P) :-
    phrase(invalid_p2_seq(P),S).
invalid_p2_seq(P) --> string(P), invalid_p2_seq(P).
invalid_p2_seq(_) --> [].
*/

pair_ids(Id1-Id2) --> integer(Id1), "-", integer(Id2).
