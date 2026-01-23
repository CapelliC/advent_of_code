:- module(_, []).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- use_module(library(clpfd)).

solve(Kind, P1,P2) :-
    phrase_from_file(sequence(machine,Ms),Kind),!,
    foldl(accum(part1),Ms,0,P1),
    foldl(accum(part2),Ms,0,P2).

:- meta_predicate accum(2,+,+,-).
accum(Pred,Machine,Acc,Sum) :-
    call(Pred,Machine,Steps),
    Sum is Acc + Steps.

% part 1

part1((Lights,Wirings,_Joltages),Steps) :-
    on_off_to_number(Lights,0,Target),
    length(Lights,N),
    invert_summed_bits_order(Wirings,N,Inverted),
    toggle_lights_steps([0],Target,Inverted,0,Steps).

on_off_to_number([],Num,Num).
on_off_to_number([OnOff|Rest],Acc,Num) :-
    ( OnOff = on -> Bit = 1 ; Bit = 0 ),
    Upd is Acc << 1 + Bit,
    !, on_off_to_number(Rest,Upd,Num).

invert_summed_bits_order(Wirings,N,SummedBits) :-
    findall(Inverted, (
       member(Wiring,Wirings),
       aggregate_all(sum(Bit), (member(W,Wiring), Bit is 1 << (N-W-1)), Inverted)
    ), SummedBits).

% breadth first search
toggle_lights_steps(Trails,Target,Wirings,Steps,TotSteps) :-
    setof(Mod, P^Wiring^(
       member(P,Trails),
       member(Wiring,Wirings),
       Mod is P xor Wiring,
       \+ memberchk(Mod,Trails)
    ), Toggled),
    Steps1 is Steps + 1,
    (  memberchk(Target,Toggled)
    -> TotSteps is Steps1
    ;  toggle_lights_steps(Toggled,Target,Wirings,Steps1,TotSteps)
    ).

% part 2

test_fd_encoding :-
    %           A   B     C   D     E     F
    %Wirings = [[3],[1,3],[2],[2,3],[0,2],[0,1]],
    Vs = [A,B,C,D,E,F],
    Joltages = [3,5,4,7],
    % encode Joltages eqs by hand
    3 #= E+F,
    5 #= B+F,
    4 #= C+D+E,
    7 #= A+B+D,

    max_list(Joltages,JMax),
    Vs ins 0..JMax,
    label(Vs), % problem has multiple solutions, will needs minimization
    sumlist(Vs,R),
    writeln(R:Vs).

part2((_Lights,Wirings,Joltages),Steps) :-
    length(Wirings,N),
    length(Vs,N),
    max_list(Joltages,JMax),
    Vs ins 0..JMax,
    once(sum_steps(Joltages,0,Wirings,Vs)),
    sum(Vs,#=,Steps),
    once(labeling([min(Steps)], Vs)),
    debug(day10,'~w x ~w',[Steps,Joltages]).

sum_steps([],_,_,_).
sum_steps([J|Joltages],P,Wirings,Vs) :-
    collect_wiring_factors(Wirings,P,Vs,SumVs),
    sum(SumVs,#=,J),
    Q is P+1,
    sum_steps(Joltages,Q,Wirings,Vs).

collect_wiring_factors([],_,[],[]).
collect_wiring_factors([Ws|Wirings],P,[V|Vs],[V|Rest]) :-
    memberchk(P,Ws),
    collect_wiring_factors(Wirings,P,Vs,Rest).
collect_wiring_factors([_|Wirings],P,[_|Vs],Rest) :-
    collect_wiring_factors(Wirings,P,Vs,Rest).

% machine parsing

machine((Lights,Wirings,Joltages)) -->
    "[",sequence(light,Lights),"]",
    sequence(button_wirings,Wirings),
    " {",sequence(integer,",",Joltages),"}",
    eol.

light(off) --> ".".
light(on) --> "#".

button_wirings(ButtonWirings) -->
    " (", sequence(integer,",",ButtonWirings), ")".
