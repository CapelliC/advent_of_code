:- module(day01, []).

solve(Kind,N1,N2) :-
    read_file_to_string(Kind,File,[]),
    re_foldl(rotation,'(?<d>L|R)(?<v_I>\\d+)',File,(50,0,0),(_,N1,N2),[]).

rotation(_{0:_,d:Dir,v:V},(C,P1,P2),(D,Q1,Q2)) :-
    F is V // 100,
    G is V mod 100,
    (   Dir="L"
    ->  D is (C-V) mod 100,
        (D==0 -> T1=1;T1=0), Q1 is P1 + T1,
        (C > 0, C-G =< 0 -> T2=1;T2=0), Q2 is P2 + F + T2
    ;   Dir="R"
    ->  D is (C+V) mod 100,
        (D==0 -> T1=1;T1=0), Q1 is P1 + T1,
        (C+G >= 100 -> T2=1;T2=0), Q2 is P2 + F + T2
    ).
