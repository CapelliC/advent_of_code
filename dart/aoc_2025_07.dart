//import 'package:collection/collection.dart';
import 'rc.dart';
import 'area.dart';

void main(List<String> args) {

  var t0 = Stopwatch()..start();

  Area area = aocArea(2025, 7, args);
  bool hitSplitter(Rc p) => area.at(p) == '^';

  var s = area.allRc.firstWhere((p) => area.at(p) == 'S');

  int nSplits = 0;
  var beams = <Rc>[s.down];

  while (beams.isNotEmpty) {
    var atRow = <Rc>{};
    bool splitBeam(Rc x) => area.valid(x) ? atRow.add(x) : false;

    for (var p in beams) {
      if (!area.valid(p)) {
        continue;
      }
      if (hitSplitter(p)) {
        if (!atRow.contains(p)) {
          nSplits++;
        }
        splitBeam(p.left);
        splitBeam(p.right);
      } else {
        atRow.add(p);
      }
    }
    beams.clear();
    beams.addAll(atRow.map((p) => p.down));
  }

  int p2 = 0;

  var sol = (p1: nSplits, p2: p2);
  print("$sol in ${t0.elapsedMilliseconds} ms");
}
