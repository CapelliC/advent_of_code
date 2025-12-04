import 'dart:io';
import 'dart:convert';

String aocSwiPath(int year, int day) => 
  "../swipl/capellic/adventofcode/$year/day${day < 10 ? '0$day' : day}";

Map dumpMapRange(int year, int day) => 
  jsonDecode(File("${aocSwiPath(year, day)}/dump_map_range.json").readAsStringSync());

String aocKind(int year, int day, List<String> args) =>
  "${aocSwiPath(year, day)}/${args.firstOrNull ?? 'input'}";
