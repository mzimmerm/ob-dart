//import 'dart:analysis_server';
//import 'dart:analyzer';
import 'dart:async';
import 'dart:collection';
import 'dart:convert';
import 'dart:core';
//import 'dart:developer';
//import 'dart:html';
//import 'dart:indexed_db';
//import 'dart:internal';
import 'dart:io';
import 'dart:isolate';
//import 'dart:js';
import 'dart:math';
import 'dart:mirrors';
//import 'dart:profiler';
//import 'dart:svg';
//import 'dart:typed_data';
//import 'dart:web_audio';
//import 'dart:web_gl';
//import 'dart:web_sql';

//// Helper class allows to run Dart code wrapped in emacs org mode.
////  This is a test version which is eventually embedded in elisp ob-dart.el
class Gen {
   
  // Wrapped code from the org file, between #+begin_src and #+end_src

  runSrc() {

    // Testing from command line:
    print('from print - for :results output');
    return 'returning - for :result value';

    // Live from elisp:
    //   - Code from begin_src .. end_src inserted here by elisp format.
    //   - See (format org-babel-dart-wrapper-method body)
    // %s
  }
  
  // run, allow print to stdout, and  ignore return value
  runSrcResultsOutput() {
    runSrc();
  }

  // Run the BEGIN_SRC .. END_SRC source block, ignoring print to stdout,
  // and return the value of the last expression of the source block,
  // which must be an explicit 'return' such as:
  //   'return lastExpression;'
  //
  // Should be invoked in the branch of
  //   'results_collection_type == 'value'',
  // where a toString() is called on the returned 'lastExpression'
  //  (the toString() is named 'raw' further in this comment).
  //
  // The toString() result pops up in elisp code in the 'ob-dart.el' function
  //   'org-babel-dart-evaluate'.
  // This function parses and manipulates the 'raw' string returned from here.
  //
  // The elisp parsing and manipulation of the 'raw' string
  // in 'org-babel-dart-evaluate' may be different
  // from the  'results_collection_type == 'output'' branch.
  // The elisp parsing and manipulation of the 'raw' in
  // the 'org-babel-dart-evaluate' depends on the parameters passed in
  //    :results [output|value] [raw]
  // for example,
  //   - [value raw] does no parsing or manipulation.
  //   - [value]     converts the 'raw' string to table if 'raw' look like a table
  //                 (grouped using () or {} or [] and delimited by comme),
  //                 otherwise behaves as [value raw]
  //   - [output raw] and [output] just return the 'raw'
  // So, most situations, the [retval.toString()] shows up in
  // the org RESULTS block.
  runSrcResultsValue() {
    // ignore prints to stdout
    var retval;
	  runZoned(() {
  	  retval = runSrc();
  	}, zoneSpecification: new ZoneSpecification(
    	print: (self, parent, zone, message) {
      	// Ignore argument message passed to print.
        // This causes any print invocations inside runZoned
        // to do nothing.  That achieves the goes of 'result: value'
        // not printing anything to stdout,
        //
    	}));

    // Return whatever was returned from 'runSrc()' .
    // In the context of
    return retval;
  }
}

void main(List args) {
  
  // todo 2
  new Gen().runSrcResultsOutput();
  // todo
  // print the result of the code which was running in the no-print zone.
  print('${new Gen().runSrcResultsValue()}');

  var results_collection_type = null;
  if (args != null && args.length > 0) {
    results_collection_type = args.elementAt(0);
  }
  
  if (results_collection_type == 'output') {
  	// For [:results output whatever] todo
  	new Gen().runSrcResultsOutput();
  } else if (results_collection_type == 'value') {
  	// generate this for :results value  (use return value and print it) 
  	// this works because in Dart print inside print still goes to stdout
  	print('${new Gen().runSrcResultsValue()}');
  } else {
    throw new Exception('Invalid collection type in results: ${results_collection_type}. Only one of [output/value] allowed.');
  }
  
}
