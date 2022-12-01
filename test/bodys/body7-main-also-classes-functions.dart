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
  // If NO await below, flow would continue immediately after being called,
  //   Classroom inside returned Future would be null,
  //   which is OK in this case, but if the Classroom was obtained
  //   remotely, Classroom inside Future would be null,
  //   and classroom.size would fail on null when called almost immediately.

  // The "then" WILL NOT await unless asked.
  // Prints after this call happen before Classroom ready, but otherwise
  // no null issues.
  return await Future.delayed(Duration(seconds: 4)).then((value) => Classroom());
  // In Sync Code
  // io.sleep(Duration(seconds:1));
}

main(List args) async {
  // create the future here. It can be created anywhere
  // With await inside classroomWithDelay(), the code awaits inside
  // BUT FLOW CONTINUES BELOW, BECAUSE classroomWithDelay() IS ASYNC,
  // SO THE WORLD FORKS BELOW
  print('1. body7: START');
  print('2. before futureClassroom');
  Future<Classroom> futureClassroom = classroomWithDelay();
  print('3. after futureClassroom');
  // var listMax = [10, 20, 30].reduce(max);
  // print('List max printed = ' + listMax.toString());

  print('4. before futureClassroom.then. Will wait 4s if await called below.');

  // "then((classroom) => processClassroom)" is necessary.
  // No other way to pull Classroom from Future
  //
  // WITHOUT await below, print('7. body7: END'); would be printed before classroom print (!!)
  //   other than that, classroom info is printed OK, no null issues.
  // The reason for the switch (without await below) is that futureClassroom
  //   is async (rather it's a future), and FLOW FORKS on discovering any ASYNC
  //   code or Future variable.
  await futureClassroom.then((Classroom classroom) {
    print('5. inside futureClassroom.then: Class size = ${classroom.size}, pupils=${classroom.pupils}');
    print('6. end of futureClassroom.then');
    return ('NO SHOW - returns to OS: Results: value!! List from MAIN');
  });
  print('7. body7: END');
}
