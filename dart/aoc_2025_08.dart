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
  void desc<E, C extends Iterable<E>>(String d, C c) {
    print('\n$d');
    c.forEachIndexed((i, v) => print("$i $v"));
  }

  var lines = aocLines(2025,8,args).where((line) => line.isNotEmpty).toList();
  var junctions = lines.map((line) {
    var m = RegExp(r"(\d+),(\d+),(\d+)").firstMatch(line)!;
    return (x: int.parse(m[1]!), y: int.parse(m[2]!), z: int.parse(m[3]!));
  }).toList();
  if (small) desc('junctions', junctions);

  var distances = <DistIJ>[
    for (var i = 0; i < junctions.length - 1; ++i)
      for (var j = i + 1; j < junctions.length; ++j)
        junctions.squaredDist(i, j)
  ];
  distances.sort(ExtDistIJ.compare);

  int part1() {

    List<Set<int>> circuits = [];

    Iterable<DistIJ> dp = distances.take(small ? 10 : 1000);
    if (small) {
      var j = junctions;
      desc('distances', dp.map((d) =>
        "${d.d}\t(${j[d.i].x},${j[d.i].y},${j[d.i].z}), (${j[d.j].x},${j[d.j].y},${j[d.j].z})\t:${d.i},${d.j}").toList());
    }

    for (var d in dp) {
      Set<int>? firstFound;
      for (var c in circuits) {
        var I = c.contains(d.i), J = c.contains(d.j);
        if (I || J) {
          if (firstFound == null) {
            if (!I) {
              c.add(d.i);
            }
            if (!J) {
              c.add(d.j);
            }
            firstFound = c;
          } else {
            firstFound.addAll(c);
            c.clear();
          }
        }
      }
      if (firstFound == null) {
        circuits.add(<int>{d.i, d.j});
      } else {
        circuits.removeWhere((c) => c.isEmpty);
      }
    }

    circuits.sort((a, b) => b.length - a.length);
    if (small) desc('circuits', circuits);
    
    return circuits[0].length * circuits[1].length * circuits[2].length;
  }

  int part2() {
    List<Set<int>> circuits = [];
    
    // seems to be necessary only to make the example (small) data works
    Set<int> placed = {};

    for (var (d:_,:i,:j) in distances) {
      Set<int>? firstFound;
      for (var c in circuits) {
        var I = c.contains(i), J = c.contains(j);
        if (I || J) {
          if (firstFound == null) {
            if (!I) {
              c.add(i);
              placed.add(i);
            }
            if (!J) {
              c.add(j);
              placed.add(j);
            }
            firstFound = c;
          } else {
            placed.addAll(c);
            firstFound.addAll(c);
            c.clear();
          }
        }
      }
      if (firstFound == null) {
        circuits.add(<int>{i, j});
        placed.addAll([i, j]);
      } else {
        circuits.removeWhere((c) => c.isEmpty);
        if (circuits.length == 1 && placed.length == junctions.length) {
          return junctions[i].x * junctions[j].x;
        }
      }
    }
    return -1;
  }
  
  print("part1: ${part1()}\npart2: ${part2()}\ncompleted in ${t0.elapsedMilliseconds} ms");
}
