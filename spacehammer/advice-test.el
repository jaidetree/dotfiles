;; [[file:testing.org::*Emacs Advice Test][Emacs Advice Test:1]]
(defun some-fun (&rest args)
  (string-join (cons "original" args) " "))
;; Emacs Advice Test:1 ends here
