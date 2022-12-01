import 'dart:async';
import 'dart:async';
import 'dart:math';
import 'dart:io' as io; // sleep is here

/// Complete Dart code snippet can be wrapped.

class Classroom {
  int size = 3;
  List<String> pupils = ['Andrea', 'Monica', 'Andrew'];
}

Future<Classroom> classroomWithDelay() async {
  // In Async Code
  // If NO await below, skipped and classroom returned immediately
  await Future.delayed(Duration(seconds: 4));
  return Classroom();
  // In Sync Code
  // io.sleep(Duration(seconds:1));
}

main(List args) async {
  // create the future here. It can be created anywhere
  Future<Classroom> futureClassroom = classroomWithDelay();
  var listMax = [10, 20, 30].reduce(max);
  print("WE HAVE MAIN");
  print("In output mode, all printed lines show in result");
  print("List max printed = " + listMax.toString());

  print("4 s Delay ahead, class info after");
  // No need to await, but then necessary.
  // No other way to pull Classroom from Future
  futureClassroom.then((Classroom classroom) {
    // only one statement here?
    print("Class size = ${classroom.size}, pupils=${classroom.pupils}");
    return ("Results: value!! List from MAIN");
  });
}