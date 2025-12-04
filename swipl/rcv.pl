:- module(rcv,
          [new/4
          ,map/3
          ,from_file/2
          ,from_file/3
          ,from_lines/3
          ,from_rows/2
          ,dimensions/2
          ,peek_1b/4,poke_1b/4
          ,peek_1b/3,poke_1b/3,bt_poke_1b/3
          ,peek_0b/4,poke_0b/4
          ,neighbours_offsets/4
          ,offsets/1
          ,offsets_for_length/2
          ,show/1,show/2,show/3
          ,bottom_right/3
          ,connect4/2
          ,connect8/2
          ]).

:- use_module(library(yall)).

new(Height,Width,V_default, Data) :-
  length(Rows, Height),
  maplist({Width,V_default}/[R]>>(
    findall(V_default, between(1,Width,_), Row),
    R=..[row|Row]
  ), Rows),
  Data =.. [rcv|Rows].

:- meta_predicate map(2,+,-).
map(Pred,Src,Dst) :-
  dimensions(Src,Rows*Cols),
  new(Rows,Cols,_,Dst),
  forall(peek_1b(Src,RC,V),
         ( (  var(Pred)
           -> U=_
           ;  call(Pred,V,U)
           ), poke_1b(Dst,RC,U))).

dimensions(Data, Rows*Cols) :-
  functor(Data,rcv,Rows),
  arg(1,Data,First),
  functor(First,row,Cols),
  forall(arg(_,Data,Row), functor(Row,row,Cols)).

from_file(File, Data) :-
  from_file(File, Data, codes),
  dimensions(Data, _*_).

from_file(File, Data, What) :-
  read_file_to_string(File,String,[]),
  string_lines(String,LinesS),
  from_lines(LinesS, Data, What).

from_lines(LinesS, Data, What) :-
  (   What = codes
  ->  maplist(string_codes,LinesS,LinesC),
      maplist([Line,Row]>>(Row=..[row|Line]),LinesC,Rows)
  ;   What = strings
  ->  maplist([S,str(S)]>>true,LinesS,Rows)
  ),
  Data =.. [rcv|Rows].

from_file_(File, Data) :-
  setup_call_cleanup(open(File,read,In),
                     findall(Line,stream_line_codes(In,Line),Lines),
                     close(In)),
  maplist([Line,Row]>>(Row=..[row|Line]),Lines,Rows),
  Data =.. [rcv|Rows].
stream_line_codes(In,Line) :-
  repeat,
  (   read_line_to_codes(In,Line0),
      Line0 \== end_of_file
  ->  Line0 = Line
  ;   !,
      fail
  ).

from_rows(Rs, RCV) :-
  maplist([Cs,Row]>>(Row=..[row|Cs]),Rs,Rows),
  RCV=.. [rcv|Rows].

peek_1b(Data, R,C, V) :-
  arg(R,Data,T),
  arg(C,T,V).
poke_1b(Data, R,C, V) :-
  arg(R,Data,T),
  nb_setarg(C,T,V).

peek_1b(Data, A*B+R*C, V) :-
  ground(A*B+R*C),
  Y is A+R,
  X is B+C,
  peek_1b(Data,Y*X, V).
peek_1b(Data, R*C, V) :-
  arg(R,Data,T),
  arg(C,T,V).
peek_1b(Data, Coords, V) :-
  is_list(Coords),
  maplist({Data}/[C,U]>>peek_1b(Data,C,U), Coords, V).

poke_1b(Data, R*C, V) :-
  arg(R,Data,T),
  nb_setarg(C,T,V).
bt_poke_1b(Data, R*C, V) :-
  arg(R,Data,T),
  setarg(C,T,V).

peek_0b(Data, R,C, V) :-
  S is R+1,
  D is C+1,
  arg(S,Data,T),
  arg(D,T,V).
poke_0b(Data, R,C, V) :-
  S is R+1,
  D is C+1,
  arg(S,Data,T),
  nb_setarg(D,T,V).

offsets([
         (-1,-1),(-1,0),(-1,+1),
         ( 0,-1),/*rcv*/( 0,+1),
         (+1,-1),(+1,0),(+1,+1)
        ]).

offsets_for_length(Length,Actuals) =>
  offsets(Os),
  maplist({Length}/[(R,C),RCs]>>
          findall((Y*X),(
                      between(1,Length,I),
                      Y is R*I,
                      X is C*I),RCs), Os,Actuals).

% note: no check for validity (just in case, peek/poke will fail)
neighbours_offsets(R,C, Rn,Cn) :-
  offsets(L),
  member((Dr,Dc),L),
  Rn is R+Dr,
  Cn is C+Dc.

show(Data) :-
  show(Data, current_output).
show(Data,Stream) :-
  show(Data,'~s\n',Stream).
show(Data,Format,Stream) :-
  forall(arg(_,Data,Row), (
             Row=..[row|Cols],
             format(Stream,Format,Cols))).

bottom_right(Data, R,C) :-
  functor(Data,rcv, R),
  arg(R,Data,LastRow),
  functor(LastRow,row, C).

connect4(R*C,R*D) :- D is C+1.
connect4(R*C,S*C) :- S is R+1.
connect4(R*C,R*D) :- D is C-1.
connect4(R*C,S*C) :- S is R-1.

connect8(RC,SD) :- connect4(RC,SD).
connect8(R*C,S*D) :- S is R-1, D is C-1.
connect8(R*C,S*D) :- S is R-1, D is C+1.
connect8(R*C,S*D) :- S is R+1, D is C-1.
connect8(R*C,S*D) :- S is R+1, D is C+1.

l_move(R*C,R*D) :- D is C-1.
u_move(R*C,S*C) :- S is R-1.
r_move(R*C,R*D) :- D is C+1.
d_move(R*C,S*C) :- S is R+1.

c_move(0'<,R*C,R*D) :- D is C-1.
c_move(0'^,R*C,S*C) :- S is R-1.
c_move(0'>,R*C,R*D) :- D is C+1.
c_move(0'v,R*C,S*C) :- S is R+1.

move_offset(0'<,  0 * -1).
move_offset(0'^, -1 *  0).
move_offset(0'>,  0 *  1).
move_offset(0'v,  1 *  0).

rc_sum(A*B,C*D,E*F):- E is A+C, F is B+D.
rc_sub(A*B,C*D,E*F):- E is A-C, F is B-D.

sum2(As,K,Bs) :-
  maplist({K}/[A,B]>>rc_sum(A,K,B),As,Bs).
sub2(As,K,Bs) :-
  maplist({K}/[A,B]>>rc_sub(A,K,B),As,Bs).

valid(R*C,Map) :-
  dimensions(Map,H*W),
  R>=1,R=<H,
  C>=1,C=<W.

rc_in_hw(R*C,H*W) :-
  R>=1,R=<H,
  C>=1,C=<W.
