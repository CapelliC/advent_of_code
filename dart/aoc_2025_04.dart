import 'rc.dart';
import 'area.dart';

void main(List<String> args) {

  var t0 = Stopwatch()..start();
  Area area = aocArea(2025, 4, args);

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
