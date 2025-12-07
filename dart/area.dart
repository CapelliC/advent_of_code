import 'aoc_swi_path.dart';
import 'rc.dart';

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

Area aocArea(int year, int day, List<String> args) => aocLines(year, day, args).where((ln) => ln.isNotEmpty).map((ln) => ln.split('')).toList();
