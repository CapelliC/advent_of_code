:- module(day05, []).

solve(Kind, P1,P2) :-
    read_file_to_string(Kind,String,[]),
    string_lines(String,Lines),
    append([FreshIdsRangesStr,[""],AvailIdsStr],Lines),
    maplist(parse_pair_ids,FreshIdsRangesStr,FreshIdsRanges),
    maplist(number_string,AvailIds,AvailIdsStr),
    !,

    aggregate_all(count,(
      member(C,AvailIds),
      check_fresh_id(C,FreshIdsRanges)
    ),P1),

    sort(FreshIdsRanges,SortedRanges),
    clear_ranges(SortedRanges,Clear),
    aggregate_all(sum(D), (member(L-H,Clear),D is H-L+1), P2).

clear_ranges([L],[L]).
clear_ranges([R1,R2|Rs], T) :-
    (   clear_range(R1,R2,Rc)
    ->  clear_ranges([Rc|Rs], T)
    ;   clear_ranges([R2|Rs], T1),
        T = [R1|T1]
    ), !.

clear_range(A1-B1,A2-B2,C1-C2) :-
    (   A2 =< B1
    ->  C1 = A1,
        (   B1 < B2
        ->  C2 = B2
        ;   C2 = B1
        )
    ).

check_fresh_id(C,FreshIdsRanges) :-
    member(A-B,FreshIdsRanges),
    between(A,B,C), !.

parse_pair_ids(Str,Id1-Id2) :-
    re_matchsub('(?<id1_I>\\d+)-(?<id2_I>\\d+)', Str,_{0:_,id1:Id1,id2:Id2}).
