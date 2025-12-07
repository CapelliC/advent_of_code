typedef Rc = (int, int);
typedef Rcs = Iterable<Rc>;

extension RcExt on Rc {
  Rc get left   => ($1, $2 - 1);
  Rc get up     => ($1 - 1, $2);
  Rc get right  => ($1, $2 + 1);
  Rc get down   => ($1 + 1, $2);

  Rc get leftUp     => ($1 - 1, $2 - 1);
  Rc get upRight    => ($1 - 1, $2 + 1);
  Rc get rightDown  => ($1 + 1, $2 + 1);
  Rc get downLeft   => ($1 + 1, $2 - 1);

  int get r => $1;
  int get c => $2;

  Rcs get connect4 sync* {
    yield left;
    yield up;
    yield right;
    yield down;
  }
  Rcs get connect8 sync* {
    yield* connect4;
    yield leftUp;
    yield upRight;
    yield rightDown;
    yield downLeft;
  }
}
