import 'dart:io';
import 'aoc_swi_path.dart';

void main(List<String> args) {

  var t0 = Stopwatch()..start();
  var instructions = File(aocKind(2025, 1, args)).readAsStringSync().split('\n');
  var sol = instructions.fold((50,0,0), (acc, instruction) {
    var (c, p1, p2) = acc;
    int d = -1, q1 = -1, q2 = -1,
        v = int.parse(instruction.substring(1)),
        f = v ~/100, g = v % 100;
    switch (instruction[0]) {
      case 'L':
        d = (c - v) % 100;
        q1 = p1 + d == 0 ? 1 : 0;
        q2 = p2 + f + (c > 0 && c - g <= 0 ? 1 : 0);
      case 'R':
        d = (c + v) % 100;
        q1 = p1 + d == 0 ? 1 : 0;
        q2 = p2 + f + (c + g >= 100 ? 1 : 0);
      default:
        throw Exception("invalid rotation ($instruction)");
    }
    return (d, q1, q2);
  });

  print("p1:${sol.$2} p2:${sol.$3} in ${t0.elapsedMilliseconds} ms");
}
