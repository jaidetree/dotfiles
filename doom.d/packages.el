;;; $DOOMDIR/packages.el -*- no-byte-compile: t; -*-

(package! fish-mode)
(package! elvish-mode)
;;(package! zsh-mode)

(package! sql-indent)

; (package! anakondo)
(package! inf-clojure)

(package! evil-lisp-state)

(package! tmux-pane)

(unpin! org-roam)

;; (package! org-pretty-table-mode
;;  :recipe (:host github :repo "Fuco1/org-pretty-table"))
;; (package! org-pretty-tags)
(package! ox-gfm)
(package! org-graph-view
  :recipe (:host github
           :repo "alphapapa/org-graph-view"))

(package! hl-fill-column)

(package! ob-typescript)

(package! command-log-mode)

(unpin! lsp-mode)

(package! monroe)

(package! prettier)

(package! reason-mode)

(package! corfu)

(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

(package! janet-mode)
