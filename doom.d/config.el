;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Jay Zawrotny"
      user-mail-address "jayzawrotny@gmail.com")

(setq display-line-numbers-type 'relative)

(setq doom-theme 'doom-one)

(setq!
 doom-font                (font-spec :family "operator mono" :size 14 :weight 'medium)
 doom-variable-pitch-font (font-spec :family "avenir next" :size 14 :weight 'medium :slant 'italic))

(custom-set-faces!
  '(line-number
    :foreground "#888")
  '(line-number-current-line
    :foreground "#ebbd80"))

(setq
 tab-always-indent                   t
 make-backup-files                   nil
 create-lockfiles                    nil
 uniquify-buffer-name-style          'post-forward-angle-brackets
 +ivy-buffer-preview                 t
 save-interprogram-paste-before-kill t
 enable-local-variables              :all
 evil-move-beyond-eol                t
 evil-split-window-below             t
 evil-vsplit-window-right            t)

(setq org-directory "~/org/roam")

(defun j/sort (pred errors)
  (sort errors pred))

(defun j/persp-name ()
  (or (safe-persp-name (get-current-persp))
      "main"))

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

(map! :nv "s-;" #'comment-or-uncomment-region)

(map! :map evil-window-map
      "/" #'evil-window-vsplit
      "-" #'evil-window-split
      "x" #'kill-buffer-and-window)

(use-package! clojure-mode
  :custom (clojure-toplevel-inside-comment-form t))

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

(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(use-package! js2-mode
  :config
  (setq
    js2-basic-offset 2
    js-expr-indent-offset -2
    js-chain-indent t))

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

(defun copy-project-path ()
  "Copies the current buffer path from the project root to copy a relative path"
  (interactive)
  (let* ((file-path (buffer-file-name))
         (project-path (or (doom-project-root) ""))
         (rel-path (replace-regexp-in-string (regexp-quote project-path) "" file-path nil 'literal)))
    (kill-new rel-path)))

(map!
  :leader
  :desc "Font zoom" "z" #'+hydra/text-zoom/body)

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

(use-package! evil-lisp-state
  :init
  (setq evil-lisp-state-global t)
  :config
  (map!
    :map evil-lisp-state-map
    ";" (evil-lisp-state-enter-command wrap-comment))
  (map! :leader :desc "Lisp" "k" evil-lisp-state-map))

(after! doom-modeline
  (custom-set-faces!
    '(doom-modeline-evil-operator-state :foreground "#FF9F9E")))

(defun j/evil-state-fg (state)
  (let ((sym (intern (concat "doom-modeline-evil-" state "-state"))))
    (face-foreground sym nil t)))

(add-hook! 'doom-load-theme-hook
    (defun j/theme-evil-cursors ()
      (setq
       evil-insert-state-cursor   (list 'bar (j/evil-state-fg "insert"))
       evil-normal-state-cursor   (list 'box (j/evil-state-fg "normal"))
       evil-visual-state-cursor   (list 'box (j/evil-state-fg "visual"))
       evil-operator-state-cursor (list 'box (j/evil-state-fg "operator"))
       evil-lisp-state-cursor     (list 'box (j/evil-state-fg "emacs"))
       evil-vterm-state-cursor    (list 'box (face-foreground 'error nil t)))))

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

(after! persp-mode
  (+workspace-current-name))

(defadvice! j/workspace-new (workspace-new &optional _ clone-p)
  "Prompt for workspace name after creating the workspace, delete if no name entered"
  :around #'+workspace/new
  (interactive "iP")
  (let ((name (read-string "Workspace name: "
                           (format "#%s" (+workspace--generate-id)))))
    (when name
      (funcall workspace-new name clone-p))))

(defun j/active-guide (level responsive display)
  (when (eq responsive 'top)
    (highlight-indent-guides--highlighter-default
      level responsive display)))

(after! highlight-indent-guides
  (setq!
    highlight-indent-guides-auto-enabled         nil
    highlight-indent-guides-responsive           'top
    highlight-indent-guides-delay                0
    highlight-indent-guides-highlighter-function 'j/active-guide)
  (custom-set-faces!
    '(highlight-indent-guides-top-character-face
       :foreground "#DE5356")))

(add-hook! '(lisp-mode-hook emacs-lisp-mode-hook clojure-mode-hook)
    (defun +disable-indent-guides-in-lisp ()
      (highlight-indent-guides-mode -1)))

(after! tramp
  ;; (setenv "SHELL" "/usr/local/bin/fish")
  (setq tramp-default-method "sshx"))

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

(defun j/tmux-select-get-session ()
  "Get the tmux session for the given persp or select a new one"
  (interactive)
  (let* ((persp-key (intern (j/persp-name)))
         (session   (plist-get j/tmux-sessions persp-key)))
    (if session
        session
        (j/tmux-select-session))))

(defun j/tmux-send-region (beg end &optional append-return)
  "Send region to tmux."
  (interactive "rP")
  (j/tmux-run (buffer-substring-no-properties beg end)
              append-return))

(defun j/tmux-send-paragraph ()
  "Send current paragraph to the selected tmux session"
  (interactive)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'paragraph)
    (j/tmux-send-region beg end t)))

(defun j/tmux-send-src-block ()
  "Send current src block to selected tmux session"
  (interactive)
  (org-babel-when-in-src-block
   (let* ((info (org-babel-get-src-block-info))
          (body (nth 1 info)))
     (j/tmux-run body t))))

(after! persp-mode
  (setq j/tmux-sessions '()
        j/tmux-history '())
  (map! :leader
        (:prefix ("e" . "tmux")
         :desc "select-session"      "s" #'j/tmux-select-session
         :desc "tmux-send-region"    "r" #'j/tmux-send-region
         :desc "tmux-send-paragraph" "p" #'j/tmux-send-paragraph
         :desc "tmux-send-src-block" "e" #'j/tmux-send-src-block)))

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

(defun vterm-buffer-change ()
  (when (derived-mode-p 'vterm-mode)
    (vterm-enter)))

(defadvice! j/vterm-project-root (toggle-vterm arg)
  "Change vterm directory project root"
  :around #'+vterm/toggle
  (let* ((default-directory (or (doom-project-root)
                              default-directory)))
    (funcall toggle-vterm arg)))

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

(after! org-roam
  (setq! org-roam-capture-templates
   (list
    '("d" "default" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "${dir}%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n"
      :unnarrowed t))))

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

(remove-hook 'text-mode-hook #'visual-line-mode)

(add-hook 'text-mode-hook #'auto-fill-mode)

(after! org (add-hook 'org-mode-hook 'turn-on-flyspell))

(after! org
  (defun unpackaged/org-element-descendant-of (type element)
    "Return non-nil if ELEMENT is a descendant of TYPE.
TYPE should be an element type, like `item' or `paragraph'.
ELEMENT should be a list like that returned by `org-element-context'."
    ;; MAYBE: Use `org-element-lineage'.
    (when-let* ((parent (org-element-property :parent element)))
      (or (eq type (car parent))
          (unpackaged/org-element-descendant-of type parent)))))

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

(setq! global-org-pretty-table-mode t)

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

(eval-after-load "org"
'(require 'ox-gfm nil t))

(setq!
 org-cycle-separator-lines 1
 org-blank-before-new-entry '((heading .         t)
                              (plain-list-item . auto)))

(add-hook! 'org-mode-hook #'+zen/toggle)

;;;###autoload
(autoload 'hl-fill-column-mode "hl-fill-column" nil t)

(map! :leader
      (:prefix ("t" . "toggle")
               :desc "fill column" "c" #'hl-fill-column-mode))

(add-hook! 'prog-mode-hook #'hl-fill-column-mode)

(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((typescript . t)
     )))

(defun j/lsp-breadcrumbs-setup ()
  (setq! lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package! lsp-mode
  :defer t
  :hook (lsp-mode . j/lsp-breadcrumbs-setup))

(map!
  :after evil
  :nmv [remap evil-previous-line] #'evil-previous-visual-line
  :nmv [remap evil-next-line] #'evil-next-visual-line)

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

(defvar j-css-base-font-size 16.0
  "Used by the px->rem function to convert px units into rem in css. Be sure to
add .0 to get accurate calculations, and this can be overwritten for a specific
buffer or project with dir-locals.el.")

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

(map! :localleader
      :map css-mode-map
      :after css-mode
      "j" #'px->rem)

(map! :leader "SPC" #'counsel-M-x)
(map! "s-P" #'counsel-M-x)
(map! "s-p" #'counsel-find-file)

(setq! tab-width 2
       indent-tabs-mode t)

(after! fennel-mode
  (setq! fennel-keywords
         '("require-macros" "eval-compiler" "defn" "doc" "lua" "hashfn" "macro" "macros"
           "import-macros" "pick-args" "pick-values" "macroexpand" "macrodebug"
           "do" "values" "if" "when" "each" "for" "fn" "lambda" "λ" "partial" "while"
           "set" "global" "var" "local" "let" "tset" "set-forcibly!" "doto" "match"
           "or" "and" "true" "false" "nil" "not" "not=" "collect" "icollect"
           "." "+" ".." "^" "-" "*" "%" "/" ">" "<" ">=" "<=" "=" "#" "..." ":"
           "->" "->>" "-?>" "-?>>" "$" "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$9"
           "rshift" "lshift" "bor" "band" "bnot" "bxor" "with-open"))

  (setq! fennel-local-fn-pattern
         (rx (syntax open-parenthesis)
             (or "fn" "lambda" "λ" "defn") (1+ space)
             (group (1+ (or (syntax word) (syntax symbol) "-" "_")))))

  (setq! fennel-font-lock-keywords
         `((,fennel-local-fn-pattern 1 font-lock-variable-name-face)
           (,(rx (syntax open-parenthesis)
                 (or "fn" "lambda" "λ" "defn") (1+ space)
                 (group (and (not (any "["))
                             (1+ (or (syntax word) (syntax symbol))))))
            1 font-lock-variable-name-face)
           (,(regexp-opt fennel-keywords 'symbols) . font-lock-keyword-face)
           (,(regexp-opt fennel-builtins 'symbols) . font-lock-builtin-face)
           (,(rx (group ":" (1+ word))) 0 font-lock-builtin-face)
           (,(rx (group letter (0+ word) "." (1+ word))) 0 font-lock-type-face)))
  (put 'defn 'fennel-indent-function 'defun))

(map!
 "<H-a>"  #'mark-whole-buffer
 "<H-v>"  #'hydra-paste/evil-paste-after
 "<H-c>"  #'kill-ring-save
 "<H-s>"  #'save-buffer
 "<H-l>"  #'goto-lin
 "<H-w>"  #'delete-window
 "<H-z>"  #'undo)

(defun org-babel-execute:emacs-lisp-config (body params)
  "Execute a block of emacs-lisp code with Babel."
  (cl-letf (((symbol-function 'current-window-configuration) #'ignore)
            ((symbol-function 'set-window-configuration) #'ignore))
    (org-babel-execute:emacs-lisp body params)))

(after! org
  (add-to-list 'org-src-lang-modes '("emacs-lisp-config" . emacs-lisp)))

(after! vterm
  (set-popup-rule!
    "^*doom:vterm-popup"
    :width  88
    :side   'right
    :vslot -4
    :select t
    :quit   nil
    :ttl    nil))
