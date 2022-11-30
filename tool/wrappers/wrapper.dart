
/// Helper class allows to run Dart code in Org Babel source block.
/// This is a test version which is eventually embedded in elisp ob-dart.el
/// as `ob-dart-no-main-snippet-wrapper-metho`.
class Gen {
  /// Wrapped code from the org file:
  ///   if org file has NO MAIN, code is from between #+begin_src and #+end_src.
  ///   if org file has MAIN,    code is copied from main.
  /// Either way, async is added if the code block contains await.
  /// That is theory: for files with MAIN, we add async here if the main is async. 
  runBlock() %a {
    //   - Org code block from begin_src .. end_src inserted here by elisp format.
    //   - See `ob-dart-wrapper` and `format-spec` in wrap-body.esh and ob-dart.el
    %s
  }

  /// Run the potentially async block asynchronously, and mark this method async,
  ///   for caller to wait at the point we need the result - just before the [print]
  ///   in the flow.
  ///
  /// See the [runBlockResultsValue] for description how the async propagation
  /// and await-ing result just before print.
  runBlockResultsOutput() %a {
    runBlock();
  }

  /// Runs the BEGIN_SRC .. END_SRC source block.
  ///
  /// Uses [ZoneSpecification] to skip the print to stdout,
  /// then return the value of the last expression
  /// of the source block, which MUST have an explicit 'return' such as:
  ///
  ///   `return returnedValue;`
  ///
  /// Should be invoked in the branch of
  ///    'results_collection_type == 'value'',
  /// where a [returnedValue.toString()] is called.
  ///
  /// The [returnedValue.toString] result pops up in elisp code
  /// in the 'ob-dart.el' function
  ///    'org-babel-dart-evaluate'.
  /// This function parses and manipulates the [returnedValue.toString] returned from here.
  ///
  /// The elisp parsing and manipulation of the [returnedValue.toString] in
  /// the 'org-babel-dart-evaluate' depends on the parameters passed in
  ///    `:results [output|value] [raw]`
  /// for example, for:
  ///   - [value raw] there is no parsing or manipulation of the result.
  ///   - [value]     converts the [returnedValue.toString] to table if the string
  ///                 looks like a table (grouped using () or {} or [], delimited by comma),
  ///                 otherwise behaves as [value raw].
  ///   - [output raw] and [output] just return the [returnedValue.toString]
  /// So, most situations, the [returnedValue.toString] shows up in
  /// the org RESULTS block.
  ///
  ///
  /// Note: ASYNC DECLARATION IS NOT REQUIRED TO BE PROPAGATED AND MARKED ON CALLER.
  ///         BUT AWAIT CALL MUST BE MARKED ASYNC ON CALLER.
  ///
  ///       In other words, a call to async method:
  ///           1. "await-marked     async method call MUST mark caller async"
  ///           2. "await-non-marked async method call has CHOICE to mark caller async"
  ///
  ///   What does this note mean:
  ///         Calling ASYNC `method` inside `methodCaller`, ONLY requires
  ///       to declare
  ///            `methodCaller async {...}`
  ///        if we use await on method call:
  ///             `await method();`
  ///
  ///        - If caller calls `await method()`, flow waits at this point.
  ///        - Otherwise, caller calls `method():`
  ///             the flow continues TO CALLERS, WHO NEVER AGAIN
  ///             ARE REQUIRED to AWAIT OR DECLARE ASYNC.
  ///             THE DISADVANTAGE OF THE "Otherwise" IS THE
  ///             FLOW CONTINUES, AND THE "AWAIT" MAY NOT FINISH UNTIL
  ///             AFTER THE PROGRAM FINISHES. THAT MEANS, WHATEVER
  ///             RESULT WAS EXPECTED FROM THE LOWEST LEVEL ASYNC FUNCTION,
  ///             WILL BE NULL.
  ///
  ///  Continue flow after the call to async `runBlock`,
  ///  without wait to caller(s).
  ///
  //   The [runBlock] runs async,
  ///  but BEFORE WE PRINT IN CALLER, this thread WAITs, making async to resolve
  ///  the future [runBlock] returnedValue BACK INTO this FLOW (THREAD) before print.
 runBlockResultsValue() %a {
    var returnedValue;
    /// Runs it's [body], the function in the first argument,
    /// in a new [Zone], based on [ZoneSpecification].
    ///
    /// The [ZoneSpecification] is defined such that any [print] statement
    /// in the [body] is ignored.
    runZoned(() {
      // If we used "await runBlock()", we would also be forced to "runZoned(() async {".
      // Even if this code did not propagate the above async up to runBlockResultsValue(),
      // or further, the "await runBlock()" causes to wait here,
      // but the code flow continue,
      // This means, that the async operation immediately reaches the caller
      //    print('${ Gen().runBlockResultsValue()}');
      // the [returnedValue] is not copied from it's Future,
      // by the time of print, so the print would output [null]
      // rather then the [returnedValue].
      returnedValue = runBlock();
    }, zoneSpecification:
        ZoneSpecification(print: (self, parent, zone, message) {
      // Ignore argument message passed to print.
      // This causes any print invocations inside runZoned
      // to do nothing.  This achieves the goals of 'result: value'
      // not printing anything to stdout
    }));

    // Return the returnedValue from 'runBlock()' .
    return returnedValue;
  }
}

 main(List args) %a {
  var results_collection_type = null;
  if (args.length > 0) {
    results_collection_type = args.elementAt(0);
  }

  if (results_collection_type == 'output') {
    // For [:results output rest], [runBlock] runs non-zoned,
    // all [print] methods execute.
    %w Gen().runBlockResultsOutput();
  } else if (results_collection_type == 'value') {
    // For [:results value rest] [runBlock] runs in the print-avoid zone.
    // This ignores all [print] in [runBlock].
    // The result is passed to [print] below, which is already out of
    // the zone, and prints [runBlockResultsValue] converted [toString].
    print('${ %w Gen().runBlockResultsValue()}');
  } else {
    throw Exception(
        'Invalid collection type in results: ${results_collection_type}. Only one of [output/value] allowed.');
  }
}
