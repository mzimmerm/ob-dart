;;; testing.el --- testing file

;;; Commentary:
;; 

;;; Code:

;; 1.
;; (setq template "template %s code")
;; (setq snippet "snippet code")
;;
;; (if (string-match-p "\%s" template)
;;     (progn
;;       (print "true")
;;       (print "true again, in progn"))
;;   ;; auto-progn, no need to wrap
;;   (print "false")
;;   (print "false again, implicit progn?"))
;;
;; ;; 2. Use template (wrapper) to format body
;; (print (format template body))

;; 3. ################################################## vvvvvvvvvvvvvv

        (setq generated-file "generated-file.dart")
        (setq body "// Dart body here")
        (setq ob-dart-wrapper "Wrap %a aa %w ww %s ss")
        (setq ob-dart-wrapper-imports "imports")


(defun split-body-depending-on-structure (body)
  "Split the passed BODY into BODY-PART-TOP and BODY-PART-MAIN.

Return a list with WRAPPER, the BODY-PART-TOP and BODY-PART-MAIN
which were split from BODY.

The result of the split depends on the BODY structure (presence of main).

The WRAPPER is either the standard ob-dart wrapper, or `%s'
depending on the BODY structure.

Comments:

1. The parts which are split from BLOCK are:
   - BODY-PART-TOP  = imports, top classes, top functions.
                      Never wrapped.
   - BODY-PART-MAIN = if the main exists, the contents of the main,
                      otherwise BLOCK except imports, top classes,
                      top functions.  Caller wrap it in WRAPPER,
                      or it becomes the WRAPPER.

2. We assume that any supported structure of the Org Babel Dart source block
can be split into 2 parts, then processed with a common wrapper
to generate the Dart source that is executed.

3. The WRAPPER contains format symbols as %s, where block parts are inserted.
Currently only BODY-PART-MAIN is inserted.

4. If main function exists in BODY:
    - the BODY effectively becomes the generated Dart code
      by returning  `%s' in WRAPPER
    - only  `:results output` is supported
   Else main function does not exist in BODY:
    -  WRAPPER is either the standard ob-dart wrapper
    - `:results output' and `:results value' are supported"

  (let* (
         (main-function-line-index (string-match "^ *\([A-z0-9_<>]+\) *main *\(.*" body)))

    (if main-function-line-index
        ;; main function exists in body
        ;; ob-dart uses this Org Babel souce block unchanged, with imports and everything.
        ;; Only :results output supported
        ;; Dart code is  "import 'dart:async';", appended with body
        ;; The wrapper becomes "%s", effectively becomes the body

        (list "%s" "import 'dart:async';" body)
      ;; main function NOT in body, passed wrapper MUST be ob-dart-wrapper
      (list ob-dart-wrapper ob-dart-wrapper-imports body))))

(defun generate-dart-full-code-from (body-part-top wrapper body-part-main)
  "Create and return full Dart code as string from Org Babel source block parts.

The created string starts with unchanged string BODY-PART-TOP,
appended with WRAPPER string which contains format symbols as %s, %w, %a.
The WRAPPER is inserted with contents of format-specs at appropriate symbols,
and BODY-PART-MAIN at WRAPPER's symbol %s.

The wrapper can be just `%s' then the BODY-PART-MAIN
becomes the WRAPPER, and is appended after BODY-PART-TOP.

Comments:

This method functions the same irrespective whether the
full source block body contained the Dart `main()' method or not.

Assumes the passed BODY-PART-TOP and BODY-PART-MAIN were extracted
from the Org Babel Dart full source block,

The logic of splitting the  source block body into
BODY-PART-TOP and BODY-PART-MAIN depends on whether the
source block body contained the Dart `main()' method or not
is not part of this method."
  (concat
   body-part-top
   (format-spec
    wrapper
    `((?a . "async ") (?w . "await ") (?s . ,body-part-main))
                                        ; ignore missing symbols %a in wrapper
    nil
                                        ; split to list
    nil)))

(defun write-dart-file-from (body generated-file)
  "From BODY, create the Dart code to run, and save it in GENERATED-FILE."
  (let*
      ;; Split body into parts, and form the `generated-dart-str'
      ((parts (split-body-depending-on-structure body))
       (wrapper (nth 0 parts))        ;; ob-dart-wrapper or "%s"
       (body-part-top (nth 1 parts))  ;; ob-dart-wrapper-imports or "import dart:async"
       (body-part-main (nth 2 parts)) ;; part of body with main code or full body
       (generated-dart-str (generate-dart-full-code-from
                            body-part-top
                            wrapper
                            body-part-main)))
    (f-write-text
     generated-dart-str
     'utf-8
     generated-file)))

(write-dart-file-from body generated-file)


(provide 'testing)

;;; testing.el ends here
