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

;; 2.
(print (format template snippet))

;; 3.
if ${= ${length $*} 1} {
   type-of $1
   echo "The time is ${date $1} since EPOCH"
} {
   echo "No millis provided"
   (eval nil)
}

(provide 'testing)

;;; testing.el ends here
