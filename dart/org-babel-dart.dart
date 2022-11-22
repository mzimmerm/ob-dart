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
  
  // run, ignore print to stdout, and use return value  (which will be printed to stdout)
  runSrcResultsValue() {
    // ignore prints to stdout
    var retval;
	  runZoned(() {
  	  retval = runSrc();
  	}, zoneSpecification: new ZoneSpecification(
    	print: (self, parent, zone, message) {
      	// Ignore argument message passed to print.
    	}));
    
    return retval;
  }
}

void main(List args) {
  
  // todo 2
  new Gen().runSrcResultsOutput();     
  print('${new Gen().runSrcResultsValue()}');

  var results_collection_type = null;
  if (args != null && args.length > 0) {
    results_collection_type = args.elementAt(0);
  }
  
  if (results_collection_type == 'output') {
  	// generate this for :results output
  	new Gen().runSrcResultsOutput();
  } else if (results_collection_type == 'value') {
  	// generate this for :results value  (use return value and print it) 
  	// this works because in Dart print inside print still goes to stdout
  	print('${new Gen().runSrcResultsValue()}');
  } else {
    throw new Exception('Invalid collection type in results: ${results_collection_type}. Only one of [output/value] allowed.');
  }
  
}
