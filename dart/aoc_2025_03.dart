import 'dart:io';
import 'aoc_swi_path.dart';

void main(List<String> args) {

  var t0 = Stopwatch()..start();
  var lines = File(aocKind(2025, 3, args)).readAsStringSync().split('\n');

  (String, String) findFirstMaxAndTail(String line) {
    var currMax = line[0];
    int currIdx = 0;
    for (int i = 1; i < line.length; ++i) {
      if (line[i].compareTo(currMax) > 0) {
        currMax = line[i];
        currIdx = i;
      }
    }
    String tail = line.substring(currIdx + 1);
    return (currMax, tail);
  }

  String subsetNMaximal(int N, String line) {
    var result = "";
    for (int i = N; i > 0; --i) {
      int sparePoint = line.length - i;
      var spare = line.substring(sparePoint);
      var toScan = line.substring(0, sparePoint);
      var (max, tail) = findFirstMaxAndTail(toScan);
      line = tail + spare;
      result += max;
    }
    return result;
  }

  var sol = lines.fold((0,0,0), (curr, line) {
    if (line.isEmpty) return curr;
    var (i, p1, p2) = curr;
    return (i + 1,
      p1 + int.parse(subsetNMaximal(2, line)),
      p2 + int.parse(subsetNMaximal(12, line)));
  });

  print("p1:${sol.$2} p2:${sol.$3} in ${t0.elapsedMilliseconds} ms");
}
