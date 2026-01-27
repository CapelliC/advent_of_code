import 'aoc_swi_path.dart';

typedef Node = String;

typedef Edges = List<Node>;
typedef Graph = Map<Node, Edges>;

extension ParseGraphAdjFormat on List<String> {
  (Node,Edges) nodeEdges(String l) => (l.split(": ")[0], l.split(": ")[1].split(" "));
  Graph parse() => { for (var ne in where((l) => l.isNotEmpty).map(nodeEdges)) ne.$1 : ne.$2 };
}

extension NaiveDFS on ({Graph graph, Set<Node> visited}) {
  /* google: count all paths from source to destination

  def count_paths(graph, u, dest, visited):
      if u == dest:
          return 1
      
      count = 0
      visited[u] = True
      for v in graph[u]:
          if not visited[v]:
              count += count_paths(graph, v, dest, visited)
      
      visited[u] = False # Backtrack
      return count
  */
  int countPaths(Node u, Node dest) {
    if (u == dest) {
      return 1;
    }
    int count = 0;
    visited.add(u);
    for (var v in graph[u] ?? []) {
      if (!visited.contains(v)) {
        count += countPaths(v, dest);
      }
    }
    visited.remove(u);
    return count;
  }
}

// https://www.geeksforgeeks.org/dsa/number-of-paths-from-source-to-destination-in-a-directed-acyclic-graph/
extension ExpectedMethodOnDAG on Graph {
  int dfs(Node u, Node dest, Map<Node,int> memo) {

    if (u == dest) return 1;
    if (memo.containsKey(u)) return memo[u]!;

    int count = 0;
    for (Node v in this[u] ?? []) {
      count += dfs(v, dest, memo);
    }
    return memo[u] = count;
  }

  int countPaths(Node src, Node dest) {
    var memo = <Node,int>{};
    return dfs(src, dest, memo);
  }
}

void main(List<String> args) {
  var t0 = Stopwatch()..start();
  var graph = aocLines(2025, 11, args).parse();

  var svr2fft = graph.countPaths('svr', 'fft');
  var fft2dac = graph.countPaths('fft', 'dac');
  var dac2out = graph.countPaths('dac', 'out');

  print("p2:${svr2fft*fft2dac*dac2out} in ${t0.elapsedMilliseconds} ms");
}
