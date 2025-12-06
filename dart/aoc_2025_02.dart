import 'aoc_swi_path.dart';

void main(List<String> args) {

  bool invalidP1(String N) {
    int L = N.length;
    if (L % 2 == 0) {
      int H = L ~/ 2;
      var l = N.substring(0, H);
      var r = N.substring(H);
      return l == r;
    }
    return false;
  }
  bool invalidP2(String N) {
    int L = N.length;
    if (L < 2) return false; // at least two digits

    bool strRepeat(int t, String N) {
      if (L % t != 0) return false;
      String p = N.substring(0, t);
      for (int q = t; q < L; q += t) {
        if (p != N.substring(q, q + t)) return false;
      }
      return true;
    }

    int H = L ~/ 2 + L % 2;
    for (int t = 1; t <= H; ++t) {
      if (strRepeat(t,N)) return true;
    }
    return false;
  }

  var t0 = Stopwatch()..start();
  final ids = aocText(2025, 2, args).split(',').map((s) => s.split('-').map(int.parse).toList());

  int checkIds(bool Function(String) invalid) => ids.fold(0, (sum,pair) {
    for (int id = pair[0]; id <= pair[1]; ++id) {
      sum += invalid(id.toString()) ? id : 0;
    }
    return sum;
  });

  var sol = (p1: checkIds(invalidP1), p2: checkIds(invalidP2)); 
  print("$sol in ${t0.elapsedMilliseconds} ms");
}
