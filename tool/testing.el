;;; testing.el --- testing file

;;; Commentary:
;; 

;;; Code:

;; 1.
(setq template "template %s code")
(setq snippet "snippet code")

(if (string-match-p "\%s" template)
    (progn
      (print "true")
      (print "true again, in progn"))
  ;; auto-progn, no need to wrap
  (print "false")
  (print "false again, implicit progn?"))

;; 2. Use template (wrapper) to format body
(print (format template body))

;; 3. Test the ob-dart core functions by pasting them here.

(setq generated-file "generated-file.dart")
(setq body "// Dart body here")
(setq ob-dart-wrapper "Wrap %a aa %w ww %s ss")
(setq ob-dart-wrapper-imports "imports")



(provide 'testing)

;;; testing.el ends here
