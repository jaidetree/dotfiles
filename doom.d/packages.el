;; [[file:../dotfiles/doom.d/config.org::*Packages][Packages:1]]
;;; $DOOMDIR/packages.el -*- no-byte-compile: t; -*-
;; Packages:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Shell script packages][Shell script packages:1]]
(package! fish-mode)
(package! elvish-mode)
;;(package! zsh-mode)
;; Shell script packages:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*SQL Indentation][SQL Indentation:1]]
(package! sql-indent)
;; SQL Indentation:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Install packages][Install packages:1]]
; (package! anakondo)
(package! inf-clojure)
;; Install packages:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Install package][Install package:1]]
(package! evil-lisp-state)
;; Install package:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Install tmux-pane][Install tmux-pane:1]]
(package! tmux-pane)
;; Install tmux-pane:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Unpin org-roam][Unpin org-roam:1]]
(unpin! org-roam)
;; Unpin org-roam:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Additional packages][Additional packages:1]]
;; (package! org-pretty-table-mode
;;  :recipe (:host github :repo "Fuco1/org-pretty-table"))
;; (package! org-pretty-tags)
(package! ox-gfm)
(package! org-graph-view
  :recipe (:host github
           :repo "alphapapa/org-graph-view"))
;; Additional packages:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Enable Package][Enable Package:1]]
(package! hl-fill-column)
;; Enable Package:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Install ob-typescript][Install ob-typescript:1]]
(package! ob-typescript)
;; Install ob-typescript:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Command log mode][Command log mode:1]]
(package! command-log-mode)
;; Command log mode:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Customize fennel][Customize fennel:2]]
(package! monroe)
;; Customize fennel:2 ends here

;; [[file:../dotfiles/doom.d/config.org::*Reason for ReScript][Reason for ReScript:1]]
(package! reason-mode)
;; Reason for ReScript:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Corfu][Corfu:1]]
(package! corfu)
;; Corfu:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Tailwind LSP][Tailwind LSP:1]]
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
;; Tailwind LSP:1 ends here
