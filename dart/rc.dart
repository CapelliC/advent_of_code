import 'dart:math';

/// row and column indexing support
class RC {
  final int r, c;
  const RC(this.r, this.c);

  RC get up => RC(r - 1, c);
  RC get left => RC(r, c - 1);
  RC get down => RC(r + 1, c);
  RC get right => RC(r, c + 1);

  RC operator-(RC p) => RC(r - p.r, c - p.c);
  
  @override
  String toString() => "$r,$c";

  @override
  bool operator == (Object x) => x is RC && r == x.r && c == x.c;

  @override
  int get hashCode => Object.hash(r, c);

  double distance(final RC p) => sqrt((r - p.r) * (r - p.r) + (c - p.c) * (c - p.c));

  int manhattanDistance(final RC p) => (r - p.r).abs() + (r - p.r).abs();

  static const L = '<';
  static const U = '^';
  static const R = '>';
  static const D = 'v';
  
  RC operator[](String d) {
    switch(d) {
      case L: return left;
      case U: return up;
      case R: return right;
      case D: return down;
    }
    throw "invalid RC[$d]";
  }

  static String inverse(String dir) {
    switch(dir) {
      case L: return R;
      case U: return D;
      case R: return L;
      case D: return U;
    }
    throw "invalid $dir";
  }

  static String steerLeft(String dir) {
    switch(dir) {
      case L: return D;
      case U: return L;
      case R: return U;
      case D: return R;
    }
    throw "invalid $dir";
  }
  static String steerRight(String dir) {
    switch(dir) {
      case L: return U;
      case U: return R;
      case R: return D;
      case D: return L;
    }
    throw "invalid $dir";
  }
  
  static List<String> get adj4 => [L,U,R,D];

  static Iterable<RC> cover(final List<List> area) sync* {
    for (int r = 0; r < area.length; ++r) {
      for (int c = 0; c < area[r].length; ++c) {
        yield RC(r, c);
      }
    }
  }
/* 
  static Iterable<RC> coverStrings(final List<String> area) sync* {
    for (int r = 0; r < area.length; ++r) {
      for (int c = 0; c < area[r].length; ++c) {
        yield RC(r, c);
      }
    }
  }
*/  
/* 
  static Iterable<RC> coverG<T extends Iterable>(final List<T> area) sync* {
    //yield* area.iterator;
    for (int r = 0; r < area.length; ++r) {
      for (int c = 0; c < area[r].length; ++c) {
        yield RC(r, c);
      }
    }
  }
*/
}

/* 
abstract interface class VectorLike {
  int get length;
//  T operator [](int index);
}
extension RCIterableExtensions on List<VectorLike> {
  Iterable<RC> get within sync* {
    for (int r = 0; r < length; ++r) {
      for (int c = 0; c < this[r].length; ++c) {
        yield RC(r, c);
      }
    }
  }
}
*/
extension RCIterableExtensions on List<String> {
  Iterable<RC> get allRc sync* {
    for (int r = 0; r < length; ++r) {
      for (int c = 0; c < this[r].length; ++c) {
        yield RC(r, c);
      }
    }
  }
}
