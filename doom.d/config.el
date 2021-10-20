;; [[file:../dotfiles/doom.d/config.org::*Config][Config:1]]
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Config:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Profile Info][Profile Info:1]]
(setq user-full-name "Jay Zawrotny"
      user-mail-address "jayzawrotny@gmail.com")
;; Profile Info:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Line Numbers][Line Numbers:1]]
(setq display-line-numbers-type 'relative)
;; Line Numbers:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Theme][Theme:1]]
(setq doom-theme 'doom-one)
;; Theme:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Font][Font:1]]
(setq!
 doom-font                (font-spec :family "operator mono" :size 18 :weight 'medium)
 doom-variable-pitch-font "Bradley Hand-18:bold:normal")
;; Font:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Font][Font:2]]
(custom-set-faces!
  '(line-number
    :foreground "#888")
  '(line-number-current-line
    :foreground "#ebbd80"))
;; Font:2 ends here

;; [[file:../dotfiles/doom.d/config.org::*General var Settings][General var Settings:1]]
(setq
 make-backup-files                   nil
 create-lockfiles                    nil
 uniquify-buffer-name-style          'post-forward-angle-brackets
 +ivy-buffer-preview                 t
 save-interprogram-paste-before-kill t
 enable-local-variables              :all
 evil-move-beyond-eol                t
 evil-split-window-below             t
 evil-vsplit-window-right            t)
;; General var Settings:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Org Initialization][Org Initialization:1]]
(setq org-directory "~/org/roam")
;; Org Initialization:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Sorting collections with reverse arg order for thread macros][Sorting collections with reverse arg order for thread macros:1]]
(defun j/sort (pred errors)
  (sort errors pred))
;; Sorting collections with reverse arg order for thread macros:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Get project name][Get project name:1]]
(defun j/persp-name ()
  (or (safe-persp-name (get-current-persp))
      "main"))
;; Get project name:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Run command and return output][Run command and return output:1]]
(defun j/cmd (command &rest args)
  "Run a command and return output"
  (let* (;(args (mapcar #'shell-quote-argument (delq nil args)))
         (cmdstr (if args (apply #'format command args) command))
         (output (get-buffer-create " *cmd stdout*"))
         (errors (get-buffer-create " *cmd stderr*"))
         code)
    (message cmdstr)
    (unwind-protect
        (if (= 0 (setq code (quiet! (shell-command cmdstr output errors))))
            (with-current-buffer output
              (buffer-string))
          (error "[%d] %s $ %s (%s)"
                 code
                 cmdstr
                 (with-current-buffer errors
                   (buffer-string))
                 cmdstr))
      (and (kill-buffer output)
           (kill-buffer errors)))))
;; Run command and return output:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Hydra Paste Cycler][Hydra Paste Cycler:1]]
(defhydra hydra-paste
  (:color red
    :hint nil)
  "\n[%s(length kill-ring-yank-pointer)/%s(length kill-ring)] \
 [_C-j_/_C-k_] cycles through yanked text, [_p_/_P_] pastes the same text \
 above or below. Anything else exits."
  ("C-j" evil-paste-pop)
  ("C-k" evil-paste-pop-next)
  ("p"   evil-paste-after)
  ("P"   evil-paste-before))

(map!
  :after evil
  :nv [remap evil-paste-after] #'hydra-paste/evil-paste-after
  :nv [remap evil-paste-before] #'hydra-paste/evil-paste-before)
;; Hydra Paste Cycler:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Commenting with cmd-;][Commenting with cmd-;:1]]
(map! :nv "s-;" #'comment-or-uncomment-region)
;; Commenting with cmd-;:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Spacemacs window split bindings][Spacemacs window split bindings:1]]
(map! :map evil-window-map
      "/" #'evil-window-vsplit
      "-" #'evil-window-split
      "x" #'kill-buffer-and-window)
;; Spacemacs window split bindings:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*General config][General config:1]]
(use-package! clojure-mode
  :custom (clojure-toplevel-inside-comment-form t))
;; General config:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Lispy config][Lispy config:1]]
(map!
  :after lispy
  :map lispy-mode-map-lispy
  "[" #'lispy-brackets
  "]" #'lispy-right-nostring
  "}" #'lispy-right-nostring)

(map!
  :after lispy
  :mode lispy-mode
  :n "[" #'lispy-backward
  :n "]" #'lispy-forward)
;; Lispy config:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Org Config][Org Config:1]]
(after! org
  (setq
   diary-file                            (concat org-directory "/diary")
   org-agenda-include-diary              nil
   org-agenda-file-regexp                "\\`[^.].*\\.org'\\|[0-9]+\\.org$"
   org-agenda-timegrid-use-ampm          t
   org-journal-dir                       (concat org-directory "/journal")
   org-journal-enable-agenda-integration nil
   org-journal-file-format               "%Y%m%d.org"
   org-journal-time-format               "%-l:%M%#p"
   org-journal-carryover-items           "TODO=\"TODO\"|TODO=\"STRT\"|TODO=\"HOLD\"")
  (setq! org-agenda-files (list org-journal-dir)))
;; Org Config:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Org Use TAB to cycle through visibility states of current subtree][Org Use TAB to cycle through visibility states of current subtree:1]]
(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))
;; Org Use TAB to cycle through visibility states of current subtree:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Settings][Settings:1]]
(use-package! js2-mode
  :config
  (setq
    js2-basic-offset 2
    js-expr-indent-offset -2
    js-chain-indent t))
;; Settings:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Delete current buffer file][Delete current buffer file:1]]
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
         (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
      (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (and (configuration-layer/package-usedp 'projectile)
                (projectile-project-p))
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' successfully removed" filename)))))
;; Delete current buffer file:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Rename current buffer file][Rename current buffer file:1]]
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((prev-name (file-name-nondirectory filename))
             (dir (file-name-directory filename))
             (new-name (read-file-name "New name: (M-n prev file)" dir prev-name nil nil)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (when (and (configuration-layer/package-usedp 'projectile)
                          (projectile-project-p))
                 (call-interactively #'projectile-invalidate-cache))
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))
;; Rename current buffer file:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Copy project path][Copy project path:1]]
(defun copy-project-path ()
  "Copies the current buffer path from the project root to copy a relative path"
  (interactive)
  (let* ((file-path (buffer-file-name))
         (project-path (or (doom-project-root) ""))
         (rel-path (replace-regexp-in-string (regexp-quote project-path) "" file-path nil 'literal)))
    (kill-new rel-path)))
;; Copy project path:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Zoom font size][Zoom font size:1]]
(map!
  :leader
  :desc "Font zoom" "z" #'+hydra/text-zoom/body)
;; Zoom font size:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Font customization][Font customization:1]]
(custom-set-faces!
  '(popup-tip-face
     :background "#FD6D6E" :foreground "black"
     :weight normal :slant oblique
     :height 120)
  '(flycheck-posframe-face
     :weight normal :slant oblique)
  '(flycheck-posframe-warning-face
     :background "#ECBE7B" :foreground "black")
  '(flycheck-posframe-info-face
     :background "#7EAF54" :foreground "black")
  '(flycheck-posframe-error-face
     :background "#FD6D6E" :foreground "black"))
;; Font customization:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Format regular flycheck errors][Format regular flycheck errors:1]]
(defun j/format-flycheck-message (msg)
  (concat
    " "
    flycheck-popup-tip-error-prefix
    (flycheck-error-format-message-and-id msg)
    " "))

(defun j/flycheck-errors->string (errors)
  "Formats ERRORS messages for display. Pads left and right of message with a space"
  (let ((messages (->> errors
                       (delete-dups)
                       (mapcar #'j/format-flycheck-message)
                       (j/sort #'identity))))
    (mapconcat 'identity messages "\n")))

(defadvice! j/format-flycheck-popup (errors)
  "Add padding to errors"
  :override #'flycheck-popup-tip-format-errors
  (-> errors
    (j/flycheck-errors->string)
    (propertize 'face 'popup-tip-face)))
;; Format regular flycheck errors:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Format childframe errors][Format childframe errors:1]]
(defadvice! j/flycheck-posframe-format-error (err)
  "Pads error message"
  :override #'flycheck-posframe-format-error
  (propertize (concat
                " "
                (flycheck-posframe-get-prefix-for-error err)
                (flycheck-error-format-message-and-id err)
                " ")
    'face
    `(:inherit ,(flycheck-posframe-get-face-for-error err))) )
;; Format childframe errors:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Unwrap][Unwrap:1]]
(defun unwrap-comment ()
  "Unwrap sexp (comment ...)"
  (interactive)
  (save-excursion
    (forward-char)
    (beginning-of-sexp)
    (let ((line (string-trim (thing-at-point 'line))))
      (if (equal line "(comment")
        (cl-destructuring-bind (beg . end) (bounds-of-thing-at-point 'line)
          (lispyville-join beg end)
          (sp-backward-sexp)
          (sp-backward-sexp)))
      (sp-unwrap-sexp)
      (sp-kill-sexp)
      (indent-sexp))))
;; Unwrap:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Wrap][Wrap:1]]
(defun wrap-comment ()
  "Wrap sexp in (comment ...) and indent it"
  (interactive)
  (let ((sexp (save-excursion
                (sexp-at-point))))
    (if (or (eq sexp 'comment)
            (eq (car sexp) 'comment))
      (unwrap-comment)
      (sp-wrap-with-pair "(")
      (insert "comment\n")
      (indent-for-tab-command)
      (evil-first-non-blank))))
;; Wrap:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Use package][Use package:1]]
(use-package! evil-lisp-state
  :init
  (setq evil-lisp-state-global t)
  :config
  (map!
    :map evil-lisp-state-map
    ";" (evil-lisp-state-enter-command wrap-comment))
  (map! :leader :desc "Lisp" "k" evil-lisp-state-map))
;; Use package:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Modeline][Modeline:1]]
(after! doom-modeline
  (custom-set-faces!
    '(doom-modeline-evil-operator-state :foreground "#FF9F9E")))
;; Modeline:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Relabel keys in the which-key menu][Relabel keys in the which-key menu:1]]
(after! which-key
  (add-to-list
    'which-key-replacement-alist
    '((nil . "evil-lisp-state-") . (nil . "")))
  (add-to-list
    'which-key-replacement-alist
    '((nil . "evil-mc-") . (nil . "")))
  (add-to-list
    'which-key-replacement-alist
    '((nil . "+multiple-cursors/") . (nil . ""))))
;; Relabel keys in the which-key menu:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Trigger an early load of workspace autoload functions][Trigger an early load of workspace autoload functions:1]]
(after! persp-mode
  (+workspace-current-name))
;; Trigger an early load of workspace autoload functions:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Advise workspace-new to prompt for name and delete if bad one is generated][Advise workspace-new to prompt for name and delete if bad one is generated:1]]
(defadvice! j/workspace-new (workspace-new &optional _ clone-p)
  "Prompt for workspace name after creating the workspace, delete if no name entered"
  :around #'+workspace/new
  (interactive "iP")
  (let ((name (read-string "Workspace name: "
                           (format "#%s" (+workspace--generate-id)))))
    (when name
      (funcall workspace-new name clone-p))))
;; Advise workspace-new to prompt for name and delete if bad one is generated:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Tramp][Tramp:1]]
(after! tramp
  ;; (setenv "SHELL" "/usr/local/bin/fish")
  (setq tramp-default-method "sshx"))
;; Tramp:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Send text to tmux][Send text to tmux:1]]
(defun j/tmux-run (command &optional append-return)
  "Run COMMAND in tmux. If NORETURN is non-nil, send the commands as keypresses
but do not execute them."
  (interactive "P")
  (let* (;; (cmd (concat command (when append-return "\r\n")))
         (cmd command)
         (session (j/tmux-select-get-session))
         (tmp (make-temp-file "emacs-send-tmux" nil nil cmd)))
    ;; (message "tmux-run: text %s" cmd)
    (unwind-protect
        (progn
          (message "tmux-run")
          (message "%s" cmd)
          (message "---")
          (j/cmd "tmux load-buffer %s" tmp)
          (j/cmd "tmux paste-buffer -dpr -t %s;" session)
          (when append-return
            (j/cmd "tmux send-keys -t %s Enter;" session))
          )
      (delete-file tmp))))
;; Send text to tmux:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Get sessions from tmux][Get sessions from tmux:1]]
(defun j/tmux-sessions ()
  "Returns a lit of active tmux-sessions"
  (-> (j/cmd "tmux list-sessions %s %s" "-F" (s-wrap "#S" "\""))
    (split-string nil nil)))

(defun j/tmux-select-session ()
  "Select and update a tmux session associated with the persp"
  (interactive)
  (let* ((sessions (j/tmux-sessions))
         (persp-key (intern (j/persp-name))))
    (ivy-read "Select tmux session: " sessions
              :history j/tmux-history
              :initial-input (plist-get j/tmux-sessions persp-key)
              :action (lambda (session)
                        (setq j/tmux-sessions
                              (plist-put j/tmux-sessions persp-key session))))))
;; Get sessions from tmux:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*API to set a shared target tmux session][API to set a shared target tmux session:1]]
(defun j/tmux-select-get-session ()
  "Get the tmux session for the given persp or select a new one"
  (interactive)
  (let* ((persp-key (intern (j/persp-name)))
         (session   (plist-get j/tmux-sessions persp-key)))
    (if session
        session
        (j/tmux-select-session))))
;; API to set a shared target tmux session:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Send Region][Send Region:1]]
(defun j/tmux-send-region (beg end &optional append-return)
  "Send region to tmux."
  (interactive "rP")
  (j/tmux-run (buffer-substring-no-properties beg end)
              append-return))
;; Send Region:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Send Paragraph][Send Paragraph:1]]
(defun j/tmux-send-paragraph ()
  "Send current paragraph to the selected tmux session"
  (interactive)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'paragraph)
    (j/tmux-send-region beg end t)))
;; Send Paragraph:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Send org-mode src-block][Send org-mode src-block:1]]
(defun j/tmux-send-src-block ()
  "Send current src block to selected tmux session"
  (interactive)
  (org-babel-when-in-src-block
   (let* ((info (org-babel-get-src-block-info))
          (body (nth 1 info)))
     (j/tmux-run body t))))
;; Send org-mode src-block:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Bindings][Bindings:1]]
(after! persp-mode
  (setq j/tmux-sessions '()
        j/tmux-history '())
  (map! :leader
        (:prefix ("e" . "tmux")
         :desc "select-session"      "s" #'j/tmux-select-session
         :desc "tmux-send-region"    "r" #'j/tmux-send-region
         :desc "tmux-send-paragraph" "p" #'j/tmux-send-paragraph
         :desc "tmux-send-src-block" "e" #'j/tmux-send-src-block)))
;; Bindings:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Configuration][Configuration:1]]
(use-package! tmux-pane
  :config
  (tmux-pane-mode)
  (map! :leader
        (:prefix ("v" . "tmux pane")
          :desc "Open vpane" :nv "o" #'tmux-pane-open-vertical
          :desc "Open hpane" :nv "h" #'tmux-pane-open-horizontal
          :desc "Open hpane" :nv "s" #'tmux-pane-open-horizontal
          :desc "Open vpane" :nv "v" #'tmux-pane-open-vertical
          :desc "Close pane" :nv "c" #'tmux-pane-close
          :desc "Rerun last command" :nv "r" #'tmux-pane-rerun))
  (map! :leader
        (:prefix "t"
          :desc "vpane" :nv "v" #'tmux-pane-toggle-vertical
          :desc "hpane" :nv "h" #'tmux-pane-toggle-horizontal))
  (map! :map org-mode-map
        :after org
        :n "C-k" #'tmux-pane-omni-window-up
        :n "C-j" #'tmux-pane-omni-window-down))
;; Configuration:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Key functions][Key functions:1]]
(defun vterm-send-esc ()
  (interactive)
  (vterm-send "ESC"))

(defun vterm-send-colon ()
  (interactive)
  (vterm-send ":"))

(defun vterm-exit ()
  (interactive)
  (evil-normal-state))

(defun vterm-enter (&rest _)
  (interactive)
  (evil-vterm-state))

(defun vterm-quit ()
  (interactive)
  (evil-window-mru)
  (vterm-exit))
;; Key functions:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Bindings][Bindings:1]]
(after! evil
  (map! "C-`" #'+vterm/toggle))

(after! vterm
  (map!
    :map vterm-mode-map
    "C-c <escape>" #'vterm-exit
    "C-c q"        #'vterm-quit
    "C-c x"        #'vterm-send-C-x
    "C-c C-d"      #'vterm-send-C-d
    "C-c :"        #'vterm-send-colon
    "C-h"          #'vterm-send-C-h
    "C-u"          #'vterm-send-C-u
    "C-]"          (cmd!! #'vterm-send-key "^]" t nil t)
    "C-^"          (cmd!! #'vterm-send-key "^" t nil t)))
;; Bindings:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Change the target vterm folder based on the file][Change the target vterm folder based on the file:1]]
(defun vterm-buffer-change ()
  (when (derived-mode-p 'vterm-mode)
    (vterm-enter)))

(defadvice! j/vterm-project-root (toggle-vterm arg)
  "Change vterm directory project root"
  :around #'+vterm/toggle
  (let* ((default-directory (or (doom-project-root)
                              default-directory)))
    (funcall toggle-vterm arg)))
;; Change the target vterm folder based on the file:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*vterm hooks][vterm hooks:1]]
(after! vterm
  (evil-define-state vterm
    "Evil vterm state.
    Used to signify when in vterm mode"
    :tag " <T> "
    :suppress-keymap t)
  (map-keymap
    (lambda (key cmd) (define-key evil-vterm-state-map (vector key) cmd))
    vterm-mode-map)
  (add-hook! 'buffer-list-update-hook #'vterm-buffer-change)
  (add-hook! 'evil-insert-state-entry-hook #'vterm-buffer-change)
  (evil-set-initial-state 'vterm-mode 'vterm))
;; vterm hooks:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*vterm hooks][vterm hooks:2]]
(defun j/decrease-vterm-font-size ()
  (text-scale-decrease 0.5))

(after! vterm
  (add-hook! 'vterm-mode-hook #'j/decrease-vterm-font-size))
;; vterm hooks:2 ends here

;; [[file:../dotfiles/doom.d/config.org::*Set mode hooks, customze directory, bind keys][Set mode hooks, customze directory, bind keys:1]]
(use-package! org-roam
  :defer t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory org-directory)
  (org-roam-tag-sources '(prop all-directories))
  :bind (:map org-roam-mode-map
         (("C-c n l" . org-roam)
          ("C-c n f" . org-roam-find-file)
          ("C-c n g" . org-roam-graph))
         :map org-mode-map
         (("C-c n i" . org-roam-insert))
         (("C-c n I" . org-roam-insert-immediate))))
;; Set mode hooks, customze directory, bind keys:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Capture templates][Capture templates:1]]
(after! org-roam
  (setq! org-roam-capture-templates
   (list
    '("d" "default" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "${dir}%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n"
      :unnarrowed t))))
;; Capture templates:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Capture][Capture:1]]
(defadvice! j/org-roam-capture (&optional goto keys)
  "Launches an `org-capture` process for a new existing note.
This uses the templates defined at `org-roam-capture-templates`.
Arguments GOTO and KEYS see `org-capture`."
  :override #'org-roam-capture
  (interactive "P")
  (unless org-roam-mode (org-roam-mode))
  (let* ((completions (org-roam--get-title-path-completions))
         (title-with-keys (org-roam-completion--completing-read "File: "
                                                                completions))
         (res (cdr (assoc title-with-keys completions)))
         (title (or (plist-get res :title) title-with-keys))
         (tags (split-string title "/"))
         (title (car (last tags)))
         (dir (string-join (butlast tags) "/"))
         (dir (if (string-blank-p dir) "" (concat dir "/")))
         (file-path (plist-get res :path)))
    (let ((org-roam-capture--info (list (cons 'title title)
                                        (cons 'slug (funcall org-roam-title-to-slug-function title))
                                        (cons 'file file-path)
                                        (cons 'dir dir)))
          (org-roam-capture--context 'capture))
      (condition-case err
          (org-roam-capture--capture goto keys)
        (error (user-error "%s.  Please adjust `org-roam-capture-templates'"
                           (error-message-string err)))))))
;; Capture:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Find File][Find File:1]]
(defadvice! j/org-roam-find-file (&optional initial-prompt completions filter-fn no-confirm)
  "launches org-roam-find-file but supports creating notes in subdirectories"
  :override #'org-roam-find-file
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let* ((completions (funcall (or filter-fn #'identity)
                               (or completions (org-roam--get-title-path-completions))))
         (title-with-tags (if no-confirm
                              initial-prompt
                            (org-roam-completion--completing-read "File: " completions
                                                                  :initial-input initial-prompt)))
         (res (cdr (assoc title-with-tags completions)))
         (title title-with-tags)
         (tags (split-string title "/"))
         (title (car (last tags)))
         (dir (string-join (butlast tags) "/"))
         (dir (if (string-blank-p dir) "" (concat dir "/")))
         (file-path (plist-get res :path)))
    (if file-path
        (org-roam--find-file file-path)
      (let ((org-roam-capture--info `((title . ,title)
                                      (slug  . ,(funcall org-roam-title-to-slug-function title))
                                      (dir   . ,dir)))
            (org-roam-capture--context 'title))
        (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
        (org-roam-capture--capture)))))
;; Find File:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Insert][Insert:1]]
(defadvice! j/org-roam-insert (&optional lowercase completions filter-fn description link-type)
  "launches org-roam-insert but supports creating notes in subdirectories"
  :override #'org-roam-insert
  (interactive "P")
  (unless org-roam-mode (org-roam-mode))
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (buffer-substring-no-properties beg end))))
               (completions (--> (or completions
                                     (org-roam--get-title-path-completions))
                                 (if filter-fn
                                     (funcall filter-fn it)
                                   it)))
               (title-with-tags (org-roam-completion--completing-read "File: " completions
                                                                      :initial-input region-text))
               (res (cdr (assoc title-with-tags completions)))
               (title (or (plist-get res :title)
                          title-with-tags))
               (tags (split-string title "/"))
               (title (car (last tags)))
               (dir (string-join (butlast tags) "/"))
               (dir (if (string-blank-p dir) "" (concat dir "/")))
               (target-file-path (plist-get res :path))
               (description (or description region-text title))
               (description (if lowercase
                                (downcase description)
                              description)))
          (cond ((and target-file-path
                      (file-exists-p target-file-path))
                 (when region-text
                   (delete-region beg end)
                   (set-marker beg nil)
                   (set-marker end nil))
                 (insert (org-roam-format-link target-file-path description link-type)))
                (t
                 (let ((org-roam-capture--info `((title . ,title-with-tags)
                                                 (dir   . ,dir)
                                                 (slug . ,(funcall org-roam-title-to-slug-function title))))
                       (org-roam-capture--context 'title))
                   (setq org-roam-capture-additional-template-props (list :region (org-roam-shield-region beg end)
                                                                          :insert-at (point-marker)
                                                                          :link-type link-type
                                                                          :link-description description
                                                                          :finalize 'insert-link))
                   (org-roam-capture--capture))))
          res))
    (deactivate-mark)) ;; Deactivate the mark on quit since `atomic-change-group' prevents it
  )
;; Insert:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Configure org-mode][Configure org-mode:1]]
(setq
 org-hide-leading-stars                          t
 org-hide-emphasis-markers                       t
 org-use-property-inheritance                    t
 org-log-done                                    'time
 org-list-allow-alphabetical                     t
 org-export-in-background                        t
 org-catch-invisible-edits                       'smart
 org-indent-indentation-per-level                2
 org-adapt-indentation                           nil
 org-indent-mode-turns-off-org-adapt-indentation t
 org-indent-mode-turns-on-hiding-stars           t
 org-re-reveal-root                              "https://cdn.jsdelivr.net/npm/reveal.js"
 org-babel-default-header-args                   '((:session .  "none")
                                                   (:results .  "replace")
                                                   (:exports .  "code")
                                                   (:cache   .  "no")
                                                   (:noweb   .  "no")
                                                   (:hlines  .  "no")
                                                   (:tangle  .  "no")
                                                   (:comments . "link")))
;; Configure org-mode:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Configure org-superstar to change org-mode bullets][Configure org-superstar to change org-mode bullets:1]]
(use-package! org-superstar
  :after org
  :config
  (setq org-superstar-remove-leading-stars t)
  ;(setq org-superstar-headline-bullets-list '("⠁" "⠃" "⠇" "⠏" "⠟" "⠿"))
  ;(setq org-superstar-headline-bullets-list '("𐄇" "𐄈" "𐄉" "𐄊" "𐄋" "𐄌" "𐄍" "𐄎" "𐄏"))
  ;(setq org-superstar-headline-bullets-list '("჻"))
  ;(setq org-superstar-headline-bullets-list '("#"))
  ;(setq org-superstar-headline-bullets-list '("∮" "∯" "∰" "∫" "∬" "∭" "⨌"))
  (setq org-superstar-headline-bullets-list '("♚" "♛" "♝" "♞" "♜" "⊱"))
  (setq org-superstar-item-bullet-alist
        '((?+ . ?•)
          (?* . ?➤)
          (?- . ?–)))
  (org-superstar-restart))
;; Configure org-superstar to change org-mode bullets:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Disable visual-line-mode and enable auto-fill-mode][Disable visual-line-mode and enable auto-fill-mode:1]]
(remove-hook 'text-mode-hook #'visual-line-mode)
;; Disable visual-line-mode and enable auto-fill-mode:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Disable visual-line-mode and enable auto-fill-mode][Disable visual-line-mode and enable auto-fill-mode:2]]
(add-hook 'text-mode-hook #'auto-fill-mode)
;; Disable visual-line-mode and enable auto-fill-mode:2 ends here

;; [[file:../dotfiles/doom.d/config.org::*Spellcheck][Spellcheck:1]]
(after! org (add-hook 'org-mode-hook 'turn-on-flyspell))
;; Spellcheck:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Defines a function to see what we are descendant of, useful for checking context][Defines a function to see what we are descendant of, useful for checking context:1]]
(after! org
  (defun unpackaged/org-element-descendant-of (type element)
    "Return non-nil if ELEMENT is a descendant of TYPE.
TYPE should be an element type, like `item' or `paragraph'.
ELEMENT should be a list like that returned by `org-element-context'."
    ;; MAYBE: Use `org-element-lineage'.
    (when-let* ((parent (org-element-property :parent element)))
      (or (eq type (car parent))
          (unpackaged/org-element-descendant-of type parent)))))
;; Defines a function to see what we are descendant of, useful for checking context:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Do-what-I-mean enter behavior][Do-what-I-mean enter behavior:1]]
;;;###autoload
(after! org
  (defun unpackaged/org-return-dwim (&optional default)
    "A helpful replacement for `org-return-indent'.  With prefix, call `org-return-indent'.

On headings, move point to position after entry content.  In
lists, insert a new item or end the list, with checkbox if
appropriate.  In tables, insert a new row or end the table."
    ;; Inspired by John Kitchin: http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
    (interactive "P")
    (if default
        (org-return t)
      (cond
       ;; Act depending on context around point.

       ;; NOTE: I prefer RET to not follow links, but by uncommenting this block, links will be
       ;; followed.

       ;; ((eq 'link (car (org-element-context)))
       ;;  ;; Link: Open it.
       ;;  (org-open-at-point-global))

       ((org-at-heading-p)
        ;; Heading: Move to position after entry content.
        ;; NOTE: This is probably the most interesting feature of this function.
        (let ((heading-start (org-entry-beginning-position)))
          (goto-char (org-entry-end-position))
          (cond ((and (org-at-heading-p)
                      (= heading-start (org-entry-beginning-position)))
                 ;; Entry ends on its heading; add newline after
                 (end-of-line)
                 (insert "\n\n"))
                (t
                 ;; Entry ends after its heading; back up
                 (forward-line -1)
                 (end-of-line)
                 (when (org-at-heading-p)
                   ;; At the same heading
                   (forward-line)
                   (insert "\n")
                   (forward-line -1))
                 ;; FIXME: looking-back is supposed to be called with more arguments.
                 (while (not (looking-back (rx (repeat 3 (seq (optional blank) "\n")))))
                   (insert "\n"))
                 (forward-line -1)))))

       ((org-at-item-checkbox-p)
        ;; Checkbox: Insert new item with checkbox.
        (org-insert-todo-heading nil))

       ((org-in-item-p)
        ;; Plain list.  Yes, this gets a little complicated...
        (let ((context (org-element-context)))
          (if (or (eq 'plain-list (car context))  ; First item in list
                  (and (eq 'item (car context))
                       (not (eq (org-element-property :contents-begin context)
                                (org-element-property :contents-end context))))
                  (unpackaged/org-element-descendant-of 'item context))  ; Element in list item, e.g. a link
              ;; Non-empty item: Add new item.
              (org-insert-item)
            ;; Empty item: Close the list.
            ;; TODO: Do this with org functions rather than operating on the text. Can't seem to find the right function.
            (delete-region (line-beginning-position) (line-end-position))
            (insert "\n"))))

       ((when (fboundp 'org-inlinetask-in-task-p)
          (org-inlinetask-in-task-p))
        ;; Inline task: Don't insert a new heading.
        (org-return t))

       ((org-at-table-p)
        (cond ((save-excursion
                 (beginning-of-line)
                 ;; See `org-table-next-field'.
                 (cl-loop with end = (line-end-position)
                          for cell = (org-element-table-cell-parser)
                          always (equal (org-element-property :contents-begin cell)
                                        (org-element-property :contents-end cell))
                          while (re-search-forward "|" end t)))
               ;; Empty row: end the table.
               (delete-region (line-beginning-position) (line-end-position))
               (org-return t))
              (t
               ;; Non-empty row: call `org-return-indent'.
               (org-return t))))
       (t
        ;; All other cases: call `org-return-indent'.
        (org-return t))))))
;; Do-what-I-mean enter behavior:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Youtube preview links in exports][Youtube preview links in exports:1]]
(after! org
  (org-link-set-parameters "yt" :export #'+org-export-yt)
  (defun +org-export-yt (path desc backend _com)
    (cond ((org-export-derived-backend-p backend 'html)
           (format "<iframe width='440' \
height='335' \
src='https://www.youtube.com/embed/%s' \
frameborder='0' \
allowfullscreen>%s</iframe>" path (or "" desc)))
          ((org-export-derived-backend-p backend 'latex)
           (format "\\href{https://youtu.be/%s}{%s}" path (or desc "youtube")))
          (t (format "https://youtu.be/%s" path)))))
;; Youtube preview links in exports:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Make org tables prettier][Make org tables prettier:1]]
(setq! global-org-pretty-table-mode t)
;; Make org tables prettier:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Make headings bigger][Make headings bigger:1]]
(custom-set-faces!
  '(outline-1 :weight regular :height 1.4)
  '(outline-2 :weight regular :height 1.3)
  '(outline-3 :weight regular :height 1.2)
  '(outline-4 :weight regular :height 1.1)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))

(after! org
  (custom-set-faces!
    '(org-document-title :height 1.4)))
;; Make headings bigger:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Symbols][Symbols:1]]
(after! org
  (set-ligatures! 'org-mode
                  :merge t
                  :checkbox      "[ ]"
                  :pending       "[-]"
                  :checkedbox    "[X]"
                  :list_property "::"
                  :results       "#+RESULTS:"
                  :property      "#+PROPERTY:"
                  :property      ":PROPERTIES:"
                  :end           ":END:"
                  :options       "#+OPTIONS:"
                  :title         "#+TITLE:"
                  :subtitle      "#+SUBTITLE:"
                  :author        "#+AUTHOR:"
                  :date          "#+DATE:"
                  :latex_class   "#+LATEX_CLASS:"
                  :latex_header  "#+LATEX_HEADER:"
                  :beamer_header "#+BEAMER_HEADER:"
                  :begin_quote   "#+BEGIN_QUOTE"
                  :end_quote     "#+END_QUOTE"
                  :begin_export  "#+BEGIN_EXPORT"
                  :end_export    "#+END_EXPORT"
                  :priority_a    "[#A]"
                  :priority_b    "[#B]"
                  :priority_c    "[#C]"
                  :priority_d    "[#D]"
                  :priority_e    "[#E]"
                  :em_dash       "---"))
;; Symbols:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Github Flavored Markdown Export][Github Flavored Markdown Export:1]]
(eval-after-load "org"
'(require 'ox-gfm nil t))
;; Github Flavored Markdown Export:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Org Mode headers margin bottom][Org Mode headers margin bottom:1]]
(setq!
 org-cycle-separator-lines 1
 org-blank-before-new-entry '((heading .         t)
                              (plain-list-item . auto)))
;; Org Mode headers margin bottom:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Zen mode][Zen mode:1]]
(add-hook! 'org-mode-hook #'+zen/toggle)
;; Zen mode:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Configure fill column][Configure fill column:1]]
;;;###autoload
(autoload 'hl-fill-column-mode "hl-fill-column" nil t)

(map! :leader
      (:prefix ("t" . "toggle")
               :desc "fill column" "c" #'hl-fill-column-mode))

(add-hook! 'prog-mode-hook #'hl-fill-column-mode)
;; Configure fill column:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Configure ob-typescript][Configure ob-typescript:1]]
(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((typescript . t)
     )))
;; Configure ob-typescript:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Display LSP breadcrumbs automatically][Display LSP breadcrumbs automatically:1]]
(defun j/lsp-breadcrumbs-setup ()
  (setq! lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package! lsp-mode
  :defer t
  :hook (lsp-mode . j/lsp-breadcrumbs-setup))
;; Display LSP breadcrumbs automatically:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Evil move between visual lines instead of logical lines][Evil move between visual lines instead of logical lines:1]]
(map!
  :after evil
  :nmv [remap evil-previous-line] #'evil-previous-visual-line
  :nmv [remap evil-next-line] #'evil-next-visual-line)
;; Evil move between visual lines instead of logical lines:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Change evil surround pairs alist :evil:evil-surround:behavior:][Change evil surround pairs alist :evil:evil-surround:behavior::1]]
(after! evil
  (setq! evil-surround-pairs-alist
         '((40 "(" . ")")
           (91 "[" . "]")
           (123 "{" . "}")
           (41 "(" . ")")
           (93 "[" . "]")
           (125 "{" . "}")
           (35 "#{" . "}")
           (98 "(" . ")")
           (66 "{" . "}")
           (62 "<" . ">")
           (116 . evil-surround-read-tag)
           (60 . evil-surround-read-tag)
           (102 . evil-surround-function))))
;; Change evil surround pairs alist :evil:evil-surround:behavior::1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Interactive function to convert px to rem][Interactive function to convert px to rem:1]]
(defvar j-css-base-font-size 16.0
  "Used by the px->rem function to convert px units into rem in css. Be sure to
add .0 to get accurate calculations, and this can be overwritten for a specific
buffer or project with dir-locals.el.")
;; Interactive function to convert px to rem:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Interactive function to convert px to rem][Interactive function to convert px to rem:2]]
(defun px->rem (px &optional beg end)
  "
  Prompts for a value in px and returns the value in rem based off
  j-css-base-font-size. Intended for use in css.
  "
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'word)))
       (list nil (car bds) (cdr bds)))))

  (let* ((has-region (and beg end))
         (txt (if has-region
                  (buffer-substring-no-properties beg end)
                px))
         px-num)
    (setq px-num
          (cond
           ((numberp txt)
            txt)
           ((string-match-p "^[0-9]+px;$" txt)
            (string-to-number (substring txt 0 -3)))
           ((string-match-p "^[0-9]+px$" txt)
            (string-to-number (substring txt 0 -2)))
           ((string-match-p "^[0-9]+$" txt)
            (string-to-number txt))
           (t ;else
            (read-number "Enter value in px: "))))
    (when has-region
      (kill-region beg end))
    (insert
     (concat
      (number-to-string (/ px-num j-css-base-font-size)) "rem;"
      " /* " (number-to-string px-num) "px */"))))
;; Interactive function to convert px to rem:2 ends here

;; [[file:../dotfiles/doom.d/config.org::*Interactive function to convert px to rem][Interactive function to convert px to rem:3]]
(map! :localleader
      :map css-mode-map
      :after css-mode
      "j" #'px->rem)
;; Interactive function to convert px to rem:3 ends here

;; [[file:../dotfiles/doom.d/config.org::*Map SPC SPC, and CMD+Shift+P to Counsel-M-x][Map SPC SPC, and CMD+Shift+P to Counsel-M-x:1]]
(map! :leader "SPC" #'execute-extended-command)
(map! "s-P" #'execute-extended-command)
(map! "s-p" #'find-file)
;; Map SPC SPC, and CMD+Shift+P to Counsel-M-x:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Use tabs for indent][Use tabs for indent:1]]
(setq! tab-width 2
       indent-tabs-mode t)
;; Use tabs for indent:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Format Config][Format Config:1]]
(setq +format-with-lsp nil)
(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode
            sql-mode
            tex-mode
            latex-mode
            org-msg-edit-mode
            js-mode
            js2-mode
            rjsx-mode
            typescript-mode
            typescript-tsx-mode))
;; Format Config:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Prettier][Prettier:2]]
(add-hook 'after-init-hook #'global-prettier-mode)
;; Prettier:2 ends here

;; [[file:../dotfiles/doom.d/config.org::*Fennel Execute][Fennel Execute:1]]
(defun org-babel-execute:fennel (body params)
  "Execute a block of fennel code with Babel."
  (org-babel-eval "fennel" body))
;; Fennel Execute:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Reason for ReScript][Reason for ReScript:2]]
(add-to-list 'auto-mode-alist '("\\.resi?$" . reason-mode))
;; Reason for ReScript:2 ends here

;; [[file:../dotfiles/doom.d/config.org::*Magit Forge Config][Magit Forge Config:1]]
(setq auth-sources '("~/.authinfo.gpg"))
;; Magit Forge Config:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Corfu][Corfu:2]]
(use-package corfu
  :hook
  (doom-first-buffer . corfu-global-mode)
  :bind (:map corfu-map
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous)))
;; Corfu:2 ends here

;; [[file:../dotfiles/doom.d/config.org::*Corfu][Corfu:3]]
(setq completion-cycle-threshold 3)
;; Corfu:3 ends here

;; [[file:../dotfiles/doom.d/config.org::*Corfu][Corfu:4]]
(setq tab-always-indent 'complete)
;; Corfu:4 ends here

;; [[file:../dotfiles/doom.d/config.org::*Corfu][Corfu:5]]
(when (equal tab-always-indent 'complete)
  (map! :map c-mode-base-map
        :i [remap c-indent-line-or-region] #'completion-at-point))
;; Corfu:5 ends here

;; [[file:../dotfiles/doom.d/config.org::*Tailwind LSP][Tailwind LSP:2]]
(use-package! lsp-tailwindcss
  :init
  (setq! lsp-tailwindcss-add-on-mode t)
  :custom
  (lsp-tailwindcss-major-modes '(rjsx-mode web-mode html-mode css-mode typescript-mode typescript-tsx-mode)))
;; Tailwind LSP:2 ends here

;; [[file:../dotfiles/doom.d/config.org::*Tailwind LSP][Tailwind LSP:3]]
(add-to-list 'lsp-language-id-configuration '(".*\\.liquid" . "html"))
;; Tailwind LSP:3 ends here

;; [[file:../dotfiles/doom.d/config.org::*Liquid][Liquid:1]]
(add-to-list 'auto-mode-alist '("\\.liquid\\'" . web-mode))
;; Liquid:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Simplify centaur-tabs buffer groups][Simplify centaur-tabs buffer groups:1]]
(defadvice! j/centaur-tabs-buffer-groups ()
  "Show all buffers within a project"
  :override #'centaur-tabs-buffer-groups
  (list (cond
         ((string-equal "*" (substring (buffer-name) 0 1)) "Emacs")
         (t (centaur-tabs-get-group-name (current-buffer))))))
;; Simplify centaur-tabs buffer groups:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Janet Language][Janet Language:2]]
(use-package! janet-mode)
;; Janet Language:2 ends here

;; [[file:../dotfiles/doom.d/config.org::*emacs-mac installation config][emacs-mac installation config:1]]
(map!
 "<H-a>"  #'mark-whole-buffer
 "<H-v>"  #'hydra-paste/evil-paste-after
 "<H-c>"  #'kill-ring-save
 "<H-s>"  #'save-buffer
 "<H-l>"  #'goto-lin
 "<H-w>"  #'delete-window
 "<H-z>"  #'undo)
;; emacs-mac installation config:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Function to execute emacs-lisp src blocks][Function to execute emacs-lisp src blocks:1]]
(defun org-babel-execute:emacs-lisp-config (body params)
  "Execute a block of emacs-lisp code with Babel."
  (cl-letf (((symbol-function 'current-window-configuration) #'ignore)
            ((symbol-function 'set-window-configuration) #'ignore))
    (org-babel-execute:emacs-lisp body params)))
;; Function to execute emacs-lisp src blocks:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Added emacs-lisp-config to org-babel using emacs-lisp as base][Added emacs-lisp-config to org-babel using emacs-lisp as base:1]]
(after! org
  (add-to-list 'org-src-lang-modes '("emacs-lisp-config" . emacs-lisp)))
;; Added emacs-lisp-config to org-babel using emacs-lisp as base:1 ends here

;; [[file:../dotfiles/doom.d/config.org::*Local config][Local config:1]]
(load! "local" doom-private-dir)
;; Local config:1 ends here
