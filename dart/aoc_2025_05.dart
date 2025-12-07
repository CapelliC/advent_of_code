import 'aoc_swi_path.dart';

// note: there is a bug, as p2 is over the correct answer by 12

typedef Range = (int, int);
extension SortableRange on Range {
  int compareTo(Range other) {
    int low = this.$1 - other.$1;
    return low == 0 ? this.$2 - other.$2 : low;
  }
  bool contains(int v) => v >= this.$1 && v <= this.$2;
  int get size => this.$2 - this.$1 + 1;
}

void main(List<String> args) {

  var t0 = Stopwatch()..start();

  var file = aocLines(2025, 5, args);
  var sepIndex = file.indexOf('');

  var freshIdsRanges = file.sublist(0, sepIndex).map((r) {
    var m = RegExp("(\\d+)-(\\d+)").firstMatch(r)!;
    return (int.parse(m[1]!), int.parse(m[2]!));
  }).toList();
  if (freshIdsRanges.isEmpty) throw Exception("no Ranges found");

  freshIdsRanges.sort((a, b) => a.compareTo(b));
  var clearRanges = <Range>[];

  var (l1, h1) = freshIdsRanges[0];
  for (int r = 1; r < freshIdsRanges.length; ++r) {
    var (l2, h2) = freshIdsRanges[r];
    if (h1 > l2) {
      if (h1 < h2) {
        h1 = h2;
      }
    } else {
      clearRanges.add((l1, h1));
      (l1, h1) = (l2, h2);
    }
  }
  clearRanges.add((l1, h1));

  var availIds = file.sublist(sepIndex + 1).where((l) => l.isNotEmpty).map(int.parse).toList();
  var sol = (
    p1: availIds.where((id) => clearRanges.where((r) => r.contains(id)).length == 1).length,
    p2: clearRanges.fold(0, (p, r) => p + r.size),
  );
  print("$sol in ${t0.elapsedMilliseconds} ms");
}
