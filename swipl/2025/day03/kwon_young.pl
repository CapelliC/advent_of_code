:- module(kwon_young, []).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

bank([H | Bank]) -->
    battery(H), sequence(battery, Bank), eol.
banks(Banks) -->
    sequence(bank, Banks).

battery(Jolt) -->
    digit(D), {number_chars(Jolt, [D])}.

% cc: Kind added by me to adapt to my input data choice
main(Kind, N, Value) :-
    phrase_from_file(banks(Banks), Kind), %"input-3.txt"),
    maplist(joltage(N), Banks, Pairs),
    maplist(maplist(label_pair), Pairs),
    maplist(concat, Pairs, Values),
    sum_list(Values, Value).

joltage(N, Bank, Selected) :-
    length(Bank, M),
    numlist(1, M, Indices),
    transpose([Bank, Indices], Tuples),
    length(Batteries, N),
    length(SelectedIndices, N),
    chain(SelectedIndices, #<),
    transpose([Batteries, SelectedIndices], Selected),
    tuples_in(Selected, Tuples).

label_pair([V, I]) :-
    labeling([max(V), ff], [V, I]).

concat(Pairs, Value) :-
    length(Pairs, N),
    numlist(1, N, L),
    reverse(L, L1),
    maplist([I, [V, _], R]>>(R is V*10^(I-1)), L1, Pairs, Values),
    sum_list(Values, Value).
