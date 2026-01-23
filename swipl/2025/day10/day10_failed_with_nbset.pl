:- module(_, []).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- use_module(library(nb_set)).

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

part2((_Lights,Wirings,Joltages),Steps) :-
    JLT =.. [jl|Joltages],
    findall(0,member(_,Joltages),Zs),
    JL0 =.. [jl|Zs],
    empty_nb_set(JL_s),
    add_nb_set(JL0,JL_s),
    increment_joltages_steps(Wirings,JLT,JL_s,0, Steps).

increment_joltages_steps(Wirings,JL_t, JL_s,Steps, StepsTot) :-
    (   debugging(part2)
    ->  size_nb_set(JL_s,N),
        debug(part2,'step ~w size ~w', [Steps,N])
    ;   true
    ),
    (  add_nb_set(JL_t,JL_s,false)
    -> StepsTot = Steps,
       !
    ;  empty_nb_set(UpdatedLevels),
       forall((
          gen_nb_set(JL_s,JL),
          member(W,Wirings),
          increment_joltage_levels(JL,W,JL_t, JL_u),
          \+ add_nb_set(JL_u,JL_s,false)
       ), add_nb_set(JL_u,UpdatedLevels)),
       Steps1 is Steps + 1,
       increment_joltages_steps(Wirings,JL_t, UpdatedLevels,Steps1,StepsTot)
    ).

increment_joltage_levels(JL,Wiring,JL_t, JL_u) :-
    increment_joltage_levels(JL,Wiring, JL_u),
    compare(Order,JL_t,JL_u),
    ( Order == > ; Order == = ).

increment_joltage_levels(JL,Wiring, JL_u) :-
    duplicate_term(JL,JL_u),
    forall(member(W,Wiring), (
       A is W+1,
       arg(A,JL,C),
       D is C+1,
       nb_setarg(A,JL_u,D)
    )).

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
