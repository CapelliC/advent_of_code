import 'package:collection/collection.dart';
import 'rc.dart';

class RCD extends RC implements Comparable<RCD> {
  final String dir;
  const RCD(super.r, super.c, this.dir);

  RCD.stay(RC p, this.dir) : super(p.r, p.c);
  RCD.move(RC p, String dir) : this.stay(p[dir], dir);

  RCD get forward => RCD.move(this, dir);
  RCD get steerLeft => RCD.move(this, RC.steerLeft(dir));
  RCD get steerRight => RCD.move(this, RC.steerRight(dir));

  @override
  String toString() => "$r,$c,$dir";

  @override
  bool operator == (Object x) =>
      x is RCD ? r == x.r && c == x.c && dir == x.dir
    : x is RC  ? r == x.r && c == x.c
    : false;

  @override
  int get hashCode => Object.hash(r, c, dir);

  RC get rc => RC(r, c);

  @override
  int compareTo(RCD other) {
    int d;
    if ((d = r - other.r) != 0) return d;
    if ((d = c - other.c) != 0) return d;
    return dir.compareTo(other.dir);
  }
}

typedef Path = List<RCD>;
extension WeightedPath on Path {
  int get cost {
    if (isEmpty) throw StateError("empty path has no cost");
    return foldIndexed(0, (int i, int v, RCD p) =>
      v + (
        i > 0 ? (this[i - 1].dir != p.dir ? 1001 : 1)
              : 0
      ));
  }
  String show(List<String> maze) {
    var byRow = <int, List<RCD>>{};
    for (var x in this) {
      if (byRow.containsKey(x.r)) {
        byRow[x.r]!.add(x);
      } else {
        byRow[x.r] = [x];
      }
    }
    String fixRow(int r, String s) {
      var row = byRow[r];
      if (row == null) return s;
      var chars = s.split("");
      for (var x in row) {
        chars[x.c] = x.dir;
      }
      return chars.join("");
    }
    return maze.mapIndexed(fixRow).join("\n ");
  }
}

extension PriorityQueueRcSet on PriorityQueue<RCD> {
  Set<RC> get toRcSet => {for (var x in unorderedElements) RC(x.r, x.c)};
}

typedef ParentsMultiple = Map<RC, List<RCD>>;
extension ParentsMultipleCostOfPath on ParentsMultiple {
  int cost(RCD target) {
    var p0 = this[target.rc];
    if (p0 == null) throw StateError("$target has no ParentsMultiple path");
    var cost = 0;
    while (p0 != null) {
      var p1 = this[p0.first.rc];
      if (p1 != null) {
        cost += 1;
        if (p0.first.dir != p1.first.dir) {
          cost += 1000;
        }
      }
      p0 = p1;
    }
    return cost;
  }
}

typedef Parents = Map<RC, RCD>;
extension ParentsCostOfPath on Parents {
  int cost(RCD target) {
    var p0 = this[target.rc];
    if (p0 == null) throw StateError("$target has no Parents path");
    var cost = 0;
    while (p0 != null) {
      var p1 = this[p0.rc];
      if (p1 != null) {
        cost += 1;
        if (p0.dir != p1.dir) {
          cost += 1000;
        }
      }
      p0 = p1;
    }
    return cost;
  }
}
