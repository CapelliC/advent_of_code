import 'aoc_swi_path.dart';

void main(List<String> args) {

  var t0 = Stopwatch()..start();

  var lines = aocLines(2025, 6, args);
  var opLine = lines.removeLast();
  var opCols = <(int, int)>[];
  var llen = opLine.length;
  for(var i = 0; i < llen; ++i) {
    if (opLine[i] != ' ') {
      var j = i + 1;
      while (j < llen && opLine[j] == ' ') {
        ++j;
      }
      opCols.add((i, j < llen ? j - 1 : j));
    }
  }

  var p1 = opCols.fold(0, (acc, opDesc) {
    var (opCol, opEnd) = opDesc;
    var nums = lines.map((line) => int.parse(line.substring(opCol, opEnd).trim()));
    return acc + (opLine[opCol] == "*" ? nums.fold(1, (c, n) => c * n) : nums.fold(0, (c, n) => c + n));
  });

  var p2 = opCols.fold(0, (acc, opDesc) {
    var (opCol, opEnd) = opDesc;
    int w = opEnd - opCol;
    List<String> numStr = List.filled(w, "");
    for (int c = 0; c < w; ++c)
      for (int r = 0; r < lines.length; ++r)
        numStr[c] += lines[r][c + opCol];
    var numInt = numStr.map((n) => int.parse(n.trim())).toList();
    return acc + (opLine[opCol] == "*" ? numInt.fold(1, (c, n) => c * n) : numInt.fold(0, (c, n) => c + n));
  });

  var sol = (p1: p1, p2: p2);
  print("$sol in ${t0.elapsedMilliseconds} ms");
}
