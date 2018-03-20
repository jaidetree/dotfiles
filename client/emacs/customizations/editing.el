;; Customizations relating to editing a buffer.

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; VIM emulation
(evil-mode 1)

;; Editor Config
(editorconfig-mode 1)

;; Unset cmd-p print key binding
(global-unset-key (kbd "s-p"))

;; Unset global cmd-k
(global-unset-key (kbd "s-k"))

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; VIM key customizations
(global-set-key (kbd "C-l") 'evil-window-right)
(global-set-key (kbd "C-h") 'evil-window-left)
(global-set-key (kbd "C-k") 'evil-window-up)
(global-set-key (kbd "C-j") 'evil-window-down)

;; Indent customizations
(global-set-key (kbd "RET") 'newline-and-indent)

;; Show file explorer
(global-set-key (kbd "s-k b") 'neotree-toggle)

;; Frame navigation
(evil-global-set-key 'normal "gt" 'other-frame)
(evil-global-set-key 'normal "gT" 'ns-prev-frame)
(global-set-key (kbd "s-{") 'ns-prev-frame)
(global-set-key (kbd "s-}") 'other-frame)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)


;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

;; Custom settings

(setq electric-indent-mode nil)

;; parinfer customizations
(setq parinfer-extensions
      '(defaults
         paredit
         smart-tab
         smart-yank))

(defun enable-parinfer-mode ()
  "Enables parinfer on indent mode + rainbow parens"
  (setq parinfer-auto-switch-indent-mode t)
  (parinfer-mode t)
  (rainbow-delimiters-mode t))

;; visualize whitespace and long-lines
(require 'whitespace)
(setq whitespace-style
      '(face
        tabs
        spaces
        trailing
        space-before-tab
        indentation
        space-after-tab
        space-mark
        tab-mark
        lines-tail))
(setq whitespace-line-column 78)
(global-whitespace-mode t)

;; delete trailling white space before save
(add-hook 'before-save-hook 'whitespace-cleanup)

