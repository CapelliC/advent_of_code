:- module(day10, []).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

solve(Kind, P1,P2) :-
    phrase_from_file(sequence(machine,Ms),Kind),!,
    maplist(part1,Ms,MinSteps),
    sumlist(MinSteps,P1),
    P2 = P1.

on_off_to_number([],Num,Num).
on_off_to_number([OnOff|Rest],Acc,Num) :-
    ( OnOff = on -> Bit = 1 ; Bit = 0 ),
    Upd is Acc << 1 + Bit,
    !, on_off_to_number(Rest,Upd,Num).

part1((Lights,Wirings,_Joltages),Steps) :-
    on_off_to_number(Lights,0,Target),
    (   Target = 0
    ->  Steps = 0
    ;   length(Lights,N),
        invert_bits_order(Wirings,N,Inverted),
        toggle_lights_steps([0],Target,Inverted,0,Steps)
    ).

% breadth first search
toggle_lights_steps(Trails,Target,Wirings,Steps,TotSteps) :-
    setof(Mod, P^Wiring^(
       member(P,Trails),
       member(Wiring,Wirings),
       toggle_bits(Wiring,P,Mod),
       \+ memberchk(Mod,Trails)
    ), Toggled),
    (  memberchk(Target, Toggled)
    -> TotSteps is Steps + 1
    ;  Steps1 is Steps + 1,
       toggle_lights_steps(Toggled,Target,Wirings,Steps1,TotSteps)
    ).

invert_bits_order(Wirings,N,Inverteds) :-
    findall(Inverted, (
      member(Wiring,Wirings),
      findall(I, (member(W,Wiring), I is N-W-1), Inverted)
    ), Inverteds).

toggle_bits([],P,P).
toggle_bits([B|Bs],P,Q) :-
    R is P xor (1 << B),
    toggle_bits(Bs,R,Q).

machine((Lights,Wirings,Joltages)) -->
    "[",sequence(light,Lights),"]",
    sequence(button_wirings,Wirings),
    " {",sequence(integer,",",Joltages),"}",
    eol.

light(off) --> ".".
light(on) --> "#".

button_wirings(ButtonWirings) -->
    " (", sequence(integer,",",ButtonWirings), ")".
