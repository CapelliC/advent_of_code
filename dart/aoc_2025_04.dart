import 'aoc_swi_path.dart';

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

typedef Area = List<List<String>>;

extension AreaExt on Area {
  Rcs get allRc sync* {
    for (int r = 0; r < length; ++r) {
      for (int c = 0; c < this[r].length; ++c) {
        yield (r, c);
      }
    }
  }

  // get or set the value at position p
  String at(Rc p, {String? v}) {
    var q = this[p.r][p.c];
    if (v != null) this[p.r][p.c] = v;
    return q;
  }

  bool valid(Rc rc) =>
    rc.$1 >= 0 && rc.$2 >= 0 && rc.$1 < length && rc.$2 < this[rc.$1].length;
}

void main(List<String> args) {

  var t0 = Stopwatch()..start();
  Area area = aocLines(2025, 4, args).where((ln) => ln.isNotEmpty).map((ln) => ln.split('')).toList();

  Rcs rollsOfPaper() =>
    area.allRc.where((rc) => area.at(rc) == '@');

  Rcs adjRollsOfPaper(Rc rc) =>
    rc.connect8.where((adj) => area.valid(adj) && area.at(adj) == '@');

  Rcs rollsAccessible() =>
    rollsOfPaper().where((rc) => adjRollsOfPaper(rc).length < 4);

  int p1 = rollsAccessible().length;

  int p2 = 0;
  for ( ; ; ) {
    var toRemove = rollsAccessible().toList();

    if (toRemove.isEmpty) break;
    p2 += toRemove.length;

    for (var r in toRemove) {
      area.at(r, v: 'x');
    }
  }

  var sol = (p1: p1, p2: p2);
  print("$sol in ${t0.elapsedMilliseconds} ms");
}
