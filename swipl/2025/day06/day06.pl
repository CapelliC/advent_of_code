:- module(day06, []).
:- use_module(capellic(rcv), % only for part 2
              [from_lines/3
              ,peek_1b/3 as at
              ,dimensions/2
              ]).

solve(Kind, P1,P2) :-
    read_file_to_string(Kind,String,[]),
    string_lines(String,Ls),
    maplist(tokens,Ls,Tks),
    append(NumsStrS,[Ops],Tks),

    findall(N,operation(NumsStrS,Ops,N),ResS),
    sumlist(ResS,P1),

    from_lines(Ls,Mat,_),
    dimensions(Mat,H*_),
    findall(Col,(at(Mat,H*Col,Code),(Code == 0'+ ; Code == 0'*)),Cols),
    maplist(vertical_digits(Mat,H),Cols,Nums2),
    maplist(list_arithmetic,Ops,Nums2,Ress2),
    sumlist(Ress2,P2).

tokens(S,Tks) :-
    split_string(S," "," ",Tks).

vertical_digits(Mat,H,Col,NumS) :-
    T is H-1,
    findall(Num,vertical_num(Mat,T,Col,Num),NumS).

vertical_num(Mat,T,Col,Num) :-
    between(Col,inf,C),
    collect_digits(Mat,T,C,NumCodes),
    ( NumCodes \== [] -> number_codes(Num,NumCodes) ; !, fail ).

collect_digits(Mat,T,Col,Digits) :-
    findall(Digit,(
      between(1,T,Row),
      at(Mat,Row*Col,Digit),
      between(0'0,0'9,Digit)
    ),Digits).

operation(NSss,Ops,Res) :-
    nth1(C,Ops,Op),
    findall(N,(
      member(NSs,NSss),
      nth1(C,NSs,S),
      number_string(N,S)
    ),Ns),
    list_arithmetic(Op,Ns,Res).

list_arithmetic("*",Ns,Res) :-
    foldl([V,C,N]>>(N is V*C),Ns,1,Res).
list_arithmetic("+",Ns,Res) :-
    foldl(plus,Ns,1,Res).
