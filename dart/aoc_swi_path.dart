import 'dart:io';

typedef ArgcArgv = List<String>;

String aocSwiPath(int year, int day) {
  var swiplPath = Platform.environment['AOC_SWI_PATH'] ?? "../swipl";
  return "$swiplPath/$year/day${day < 10 ? '0$day' : day}";
}

String aocKind(int year, int day, ArgcArgv args) =>
  "${aocSwiPath(year, day)}/${args.firstOrNull ?? 'input'}";

String aocText(int year, int day, ArgcArgv args) =>
  File(aocKind(year, day, args)).readAsStringSync();

List<String> aocLines(int year, int day, ArgcArgv args) =>
  aocText(year, day, args).split(Platform.lineTerminator);

/* sync debug configuration/description with swipl using JSON files
import 'dart:convert';
Map dumpMapRange(int year, int day) => 
  jsonDecode(File("${aocSwiPath(year, day)}/dump_map_range.json").readAsStringSync());
*/
