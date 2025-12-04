import 'rc.dart';

const L = 1 << 0; // LEFT
const U = 1 << 1; // UP
const R = 1 << 2; // RIGHT
const D = 1 << 3; // DOWN

class Mat<X> {

  Mat(this._data);
  final List<List<X>> _data;

  int borders(RC p) {
    X v = at(p);
    int borders = 0;
    if (p.r == 0 || at(p.up) != v) borders |= U;
    if (p.c == 0 || at(p.left) != v) borders |= L;
    if (p.r == height - 1 || at(p.down) != v) borders |= D;
    if (p.c == width - 1 || at(p.right) != v) borders |= R;
    return borders;
  }

  Iterable<RC> adj4(RC p) sync* {
    if (p.r > 0) yield p.up;
    if (p.c > 0) yield p.left;
    if (p.r < height - 1) yield p.down;
    if (p.c < width - 1) yield p.right;
  }

  Iterable<RC> get rectPoints sync* {
    for (int r = 0; r < height; ++r) {
      for (int c = 0; c < width; ++c) {
        yield RC(r, c);
      }
    }
  }

  // get or set the value at position p
  X at(final RC p, {X? v}) {
    var q = _data[p.r][p.c];
    if (v != null) _data[p.r][p.c] = v;
    return q;
  }

  bool rectangular() {
    final length_0 = _data[0].length;
    return _data.every((List<X> s) => s.length == length_0);
  }

  RC get size => RC(height, width);
  int get height => _data.length;
  int get width => _data[0].length;

  Mat<X> cloneFilled(X fill) =>
    Mat<X>(List<List<X>>.generate(size.r, (_) => List<X>.filled(size.c, fill)));
}
