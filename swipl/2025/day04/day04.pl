:- module(day04, []).
:- use_module(capellic(rcv),
             [from_file /2
             ,connect8  /2
             ,peek_1b   /3 as get
             ,poke_1b   /3 as set
             ]).

solve(Kind, P1,P2) :-
    from_file(Kind,RCV),

    aggregate_all(count,'rolls of paper accessible'(RCV,_),P1),

    'remove rolls of paper'(RCV,Removed),
    sumlist(Removed,P2).

'rolls of paper accessible'(RCV,P) :-
    get(RCV,P,0'@),
    aggregate_all(count,(connect8(P,Q),get(RCV,Q,0'@)),N),
    N < 4.

'remove rolls of paper'(RCV,L_Removed) :-
    findall(P,'rolls of paper accessible'(RCV,P),Removed),
    forall(member(P,Removed),set(RCV,P,0'x)),
    length(Removed,N_Removed),

    debug(day04,'~w',[N_Removed]),
    (   N_Removed > 0
    ->  'remove rolls of paper'(RCV,T_Removed),
        L_Removed = [N_Removed|T_Removed]
    ;   L_Removed = []
    ).
