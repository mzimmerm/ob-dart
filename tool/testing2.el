;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.
(setq generated-file "agen.dart")
"agen.dart"

(setq ob-dart-wrapper-imports "imports")
"imports"

(setq body "BODY")
"BODY"

(setq ob-dart-wrapper "wrap %a aa %w ww %s")
"wrap %a aa %w ww %s"


       (let* ((full-body  (concat
                                        ob-dart-wrapper-imports
                                        (format-spec
                                           ob-dart-wrapper
                                           `((?a . "async ") (?w . "await ") (?s . ,body))
                                           nil    ; ignore-missing
                                           nil) ; split to list
                                        ; "some string"
                                      ))

        )
        (f-write-text
           full-body
           'utf-8
           generated-file))

nil




