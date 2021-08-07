;; ((org-mode
;;   (eval add-hook! 'after-save-hook
;;         '(lambda () (if (y-or-n-p "Tangle?")
;;                         (org-babel-tangle)
;;                       t))  nil t)))
;;
