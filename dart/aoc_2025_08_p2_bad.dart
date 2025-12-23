import 'package:collection/collection.dart';
import 'aoc_swi_path.dart';

typedef P3d = ({int x, int y, int z});

typedef DistIJ = ({int d, int i, int j});

extension ExtP3d on List<P3d> {
  DistIJ squaredDist(int i, int j) {
    var I = this[i], J = this[j];
    var D = (I.x - J.x) * (I.x - J.x) + (I.y - J.y) * (I.y - J.y) + (I.z - J.z) * (I.z - J.z);
    return (d:D, i:i, j:j);
  }
}
extension ExtDistIJ on DistIJ {
  static int compare(DistIJ a, DistIJ b) {
    int d = a.d - b.d;
    if (d == 0) {
      int i = a.i - b.i;
      if (i == 0) {
        return a.j - b.j;
      }
      return i;
    }
    return d;
  }
}

// see https://github.com/brebs-gh/advent-of-code/blob/main/2025/aoc2025_day8.pl
//  for the algorithm in swi-prolog
void main(List<String> args) {

  var t0 = Stopwatch()..start();

  var small = args.firstOrNull == 'small';
  void desc<E, C extends List<E>>(String d, C c) {
    print('\n$d');
    c.forEachIndexed((i, v) => print("$i $v"));
  }

  var lines = aocLines(2025,8,args).where((line) => line.isNotEmpty).toList();
  var junctionBoxesP3d = lines.map((line) {
    var m = RegExp(r"(\d+),(\d+),(\d+)").firstMatch(line)!;
    return (x: int.parse(m[1]!), y: int.parse(m[2]!), z: int.parse(m[3]!));
  }).toList();
  if (small) desc('junctionBoxesP3d', junctionBoxesP3d);

  var distances = <DistIJ>[
    for (var i = 0; i < junctionBoxesP3d.length - 1; ++i)
      for (var j = i + 1; j < junctionBoxesP3d.length; ++j)
        junctionBoxesP3d.squaredDist(i, j)
  ];
  distances.sort(ExtDistIJ.compare);

  /////////// PART 1
  
  // rewrite using brebs' algorithm
  List<Set<int>> circuits = [];

  List<DistIJ> dp = distances.take(small ? 10 : 1000).toList();
  if (small) {
    // ease visual comparison with brebs' output
    var j = junctionBoxesP3d;
    desc('distances', dp.map((d) =>
      "${d.d}\t(${j[d.i].x},${j[d.i].y},${j[d.i].z}), (${j[d.j].x},${j[d.j].y},${j[d.j].z})\t:${d.i},${d.j}").toList());
  }

  while (dp.isNotEmpty) {
    var (d:_, i:p1, j:p2) = dp.removeAt(0);
    var inCircuit = <int>{p1, p2};
    var circuit = <int>{};
    while (inCircuit.isNotEmpty) {
      var p = inCircuit.first;
      inCircuit.remove(p);
      circuit.add(p);
      var s = 0;
      while (s < dp.length) {
        var (d:_, i:I, j:J) = dp[s];
        if (I == p) {
          inCircuit.add(J);
        }
        if (J == p) {
          inCircuit.add(I);
        }
        if (I == p || J == p) {
          dp.removeAt(s);
        } else {
          ++s;
        }
      }
    }
    circuits.add(circuit);
  }

  /*  imho, this simpler algorithm is a better match for day 8 part 1 description...
      but it fails to produce 40 as required

  List<Set<int>> circuits = [];
  //int n = 0;
  for (var d in distances.take(small ? 10 : 1000)) {
    var f = false;
    for (var c in circuits) {
      var I = c.contains(d.i), J = c.contains(d.j);
      if (I || J) {
        if (!I) c.add(d.i);
        if (!J) c.add(d.j);
        f = true;
        break;
      }
    }
    if (!f) {
      circuits.add(<int>{d.i, d.j});
    }

    // useless attempt to change the termination
    var t = circuits.fold(0, (int c, Set<int> s) => c + s.length);
    //if (t == junctionBoxesP3d.length || n++ == (small ? 10 : 1000)) {
    if (t == junctionBoxesP3d.length) {
      break;
    }
  }
  */

  circuits.sort((a, b) => b.length - a.length);
  if (small) desc('circuits', circuits);
  
  int p1 = circuits[0].length * circuits[1].length * circuits[2].length;

  //////////// PART 2

  var pairableShort = Set<(int, int)>.from(distances.map((d) => (d.i, d.j)));
  var unconnectedJunction = Set<int>.from(junctionBoxesP3d.mapIndexed((i, _) => i));
  (int, int) notPair = (-1, -1);
  (int, int) last2 = notPair;

  var jf = pairableShort.first;
  unconnectedJunction.remove(jf.$1);
  unconnectedJunction.remove(jf.$2);
  while (unconnectedJunction.isNotEmpty) {
    jf = notPair;
    for (var (j1, j2) in pairableShort) {
      if (unconnectedJunction.contains(j1)) {
        jf = (j1, j2);
        break;
      }
      if (unconnectedJunction.contains(j2)) {
        jf = (j2, j1);
        break;
      }
    }
    if (jf != notPair) {
      pairableShort.remove(jf);
      unconnectedJunction.remove(jf.$1);
      last2 = jf;
    }
  }
  var p2 = junctionBoxesP3d[last2.$1].x * junctionBoxesP3d[last2.$2].x;

  print("part1: $p1 part2: $p2 in ${t0.elapsedMilliseconds} ms");
}
