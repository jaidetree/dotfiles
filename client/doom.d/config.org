#+TITLE: DOOM Config
#+PROPERTY: header-args :lexical yes :results silent
#+STARTUP: content


* Tasks
** TODO Move config.el into config.org
** TODO Research and fix pressing enter on list items
** TODO Research and swap org-mode enter behavior with C-ent
** DONE Find better config for org-mode pretty symbols
CLOSED: [2020-10-11 Sun 15:56]
** KILL Experiment with a command to load a buffer into tmux and copy it to clipboard
CLOSED: [2020-10-11 Sun 15:56]
** DONE Update init.el to stop using childframe
** DONE Update config to enter into buffer select-mode when creating a split
** DONE Look into [[file:~/org/roam/20201002230128-wrap_text_in_org_mode.org][Wrap text in org-mode]]


* Usage and tips

** Additional functions/macros that could help you configure Doom

- `load!' for loading external *.el files relative to this one
- `use-package' for configuring packages
- `after!' for running code after a package has loaded
- `add-load-path!' for adding directories to the `load-path', relative to
  - this file. Emacs searches the `load-path' when you load packages with
  - `require' or `use-package'.
- `map!' for binding new keys

** Lookup symbol under cursor

To get information about any of these functions/macros, move the cursor over
  the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
  This will open documentation for it, including demos of how they are used.

** Lookup definition

You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
  they are implemented.

** Reload config
#+begin_src emacs-lisp :tangle no
(org-babel-load-file "~/.doom.d/config.org")
#+END_SRC

** Packages
To install a package with Doom you must declare them here and run ~doom sync~
on the command line, then restart Emacs for the changes to take effect -- or
use =M-x doom/reload=.


*** Installing packages
To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:

#+BEGIN_EXAMPLE emacs-lisp
(package! some-package)
#+END_EXAMPLE

*** Git Recipes
To install a package directly from a remote git repo, you must specify a
~:recipe~. You'll find documentation on what ~:recipe~ accepts here:
https://github.com/raxod502/straight.el#the-recipe-format

#+BEGIN_EXAMPLE emacs-lisp
(package! another-package
 :recipe (:host github :repo "username/repo"))
#+END_EXAMPLE

If the package you are trying to install does not contain a PACKAGENAME.el
file, or is located in a subdirectory of the repo, you'll need to specify
~:files~ in the ~:recipe~:


#+BEGIN_EXAMPLE emacs-lisp
(package! this-package
 :recipe (:host github :repo "username/repo"
          :files ("some-file.el" "src/lisp/*.el")))
#+END_EXAMPLE

*** Disable Packages
If you'd like to disable a package included with Doom, you can do so here
with the ~:disable~ property:

#+BEGIN_EXAMPLE emacs-lisp
(package! builtin-package :disable t)
#+END_EXAMPLE

*** Overriding default packages
You can override the recipe of a built in package without having to specify
all the properties for ~:recipe~. These will inherit the rest of its recipe
from Doom or MELPA/ELPA/Emacsmirror:

#+BEGIN_EXAMPLE emacs-lisp
(package! builtin-package :recipe (:nonrecursive t))
(package! builtin-package-2 :recipe (:repo "myfork/package"))
#+END_EXAMPLE

*** Targeting a specific recipe branch
Specify a ~:branch~ to install a package from a particular branch or tag.
This is required for some packages whose default branch isn't 'master' (which
our package manager can't deal withsee =raxod502/straight.el#279=)

#+BEGIN_EXAMPLE emacs-lisp
(package! builtin-package :recipe (:branch "develop"))
#+END_EXAMPLE

*** Pin a specific commit
Use ~:pin~ to specify a particular commit to install.

#+BEGIN_EXAMPLE emacs-lisp
(package! builtin-package :pin "1a2b3c4d5e")
#+END_EXAMPLE


*** Unpin a package to use latest head
Doom's packages are pinned to a specific commit and updated from release to
release. The ~unpin!~ macro allows you to unpin single packages...

#+BEGIN_EXAMPLE example
(unpin! pinned-package)
#+END_EXAMPLE

...or multiple packages

#+BEGIN_EXAMPLE emacs-lisp
(unpin! pinned-package another-pinned-package)
#+END_EXAMPLE

...Or *all* packages (NOT RECOMMENDEDwill likely break things)

#+BEGIN_EXAMPLE emacs-lisp
(unpin! t)
#+END_EXAMPLE


* Headers
** Packages
#+begin_src emacs-lisp :tangle packages.el
;;; $DOOMDIR/packages.el -*- no-byte-compile: t; -*-
#+END_SRC

** Config
#+begin_src emacs-lisp
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
#+END_SRC


* Initialization

** Profile Info
Place your private configuration here! Remember, you do not need to run ~doom
sync~ after modifying this file!
 
Some functionality uses this to identify you, e.g. GPG configuration, email
clients, file templates and snippets.

#+begin_src emacs-lisp
(setq user-full-name "Jay Zawrotny"
      user-mail-address "jayzawrotny@gmail.com")
#+END_SRC

** Line Numbers
This determines the style of line numbers in effect. If set to `nil', line
numbers are disabled. For relative line numbers, set this to `relative'.

#+begin_src emacs-lisp
(setq display-line-numbers-type 'relative)
#+END_SRC

** Theme Settings
*** Introduction
Doom exposes five (optional) variables for controlling fonts in Doom. Here are
the three important ones:

  + `doom-font'
  + `doom-variable-pitch-font'
  + `doom-big-font' -- used for `doom-big-font-mode'; use this for presentations or streaming.

They all accept either a font-spec, font string ("Input Mono-12"), or xlfd font
string. You generally only need these two:
#+BEGIN_EXAMPLE emacs-lisp
(setq
  doom-font                (font-spec :family "monospace" :size 12 :weight 'semi-light)
  doom-variable-pitch-font (font-spec :family "monospace" :size 12 :weight 'semi-light))
#+END_EXAMPLE

There are two ways to load a theme. Both assume the theme is installed and
available. You can either set =doom-theme= or manually load a theme with the

*** Theme
#+begin_src emacs-lisp
(setq doom-theme 'doom-one)
#+END_SRC

**** Change a theme at runtime
It's recommended to use doom-theme so Henrik can update how its used over time,
but this should work for loading when needed.

#+begin_src emacs-lisp :tangle no
(load-theme 'doom-one t)
#+END_SRC

*** Font
Set default font

#+begin_src emacs-lisp
(setq!
 doom-font                (font-spec :family "operator mono" :size 14 :weight 'medium)
 doom-variable-pitch-font (font-spec :family "avenir next" :size 14 :weight 'medium :slant 'italic))
#+END_SRC

Make line numbers brighter

#+begin_src emacs-lisp
(custom-set-faces!
  '(line-number
    :foreground "#888")
  '(line-number-current-line
    :foreground "#ebbd80"))
#+END_SRC



* General var Settings
- What can I say? I'm fussy.
#+begin_src emacs-lisp
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
#+END_SRC


* Org Initialization
- If you use `org' and don't want your org files in the default location below,
  change `org-directory'. It must be set before org loads!

#+begin_src emacs-lisp
(setq org-directory "~/org/roam")
#+END_SRC


* Utility Macros
** Lisp comment macro
- Similar to Clojure's. Lets you wrap any elisp code without eval'ing it.

#+begin_src emacs-lisp
(defmacro comment (&rest _)
  `nil)
#+END_SRC* Local config

** Sorting collections with reverse arg order for thread macros
#+begin_src emacs-lisp
(defun j/sort (pred errors)
  (sort errors pred))
#+END_SRC

** Get project name
#+begin_src emacs-lisp
(defun j/persp-name ()
  (or (safe-persp-name (get-current-persp))
      "main"))
#+END_SRC

** Run command and return output
#+begin_src emacs-lisp
(defun j/cmd (command &rest args)
  "Run a command and return output"
  (let* ((args (mapcar #'shell-quote-argument (delq nil args)))
         (cmdstr (if args (apply #'format command args) command))
         (output (get-buffer-create " *cmd stdout*"))
         (errors (get-buffer-create " *cmd stderr*"))
         code)
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
#+END_SRC


* Customizations

** Fish syntax support
#+begin_src emacs-lisp :tangle packages.el
(package! fish-mode)
#+END_SRC

** SQL Indentation
#+begin_src emacs-lisp :tangle packages.el
(package! sql-indent)
#+END_SRC

** Hydra Paste Cycler
- Cycle through the kill ring on paste - only for p and P in normal mode

#+begin_src emacs-lisp
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
#+END_SRC

** Commenting with cmd-;
#+begin_src emacs-lisp
(map! :nv "s-;" #'comment-or-uncomment-region)
#+END_SRC


** Spacemacs window split bindings
#+begin_src emacs-lisp
(map! :map evil-window-map
      "/" #'evil-window-vsplit
      "-" #'evil-window-split
      "x" #'kill-buffer-and-window)
#+END_SRC

** Clojure
*** Install packages
#+begin_src emacs-lisp :tangle packages.el
(package! anakondo)
(package! inf-clojure)
#+END_SRC

*** General config
#+begin_src emacs-lisp
(use-package! clojure-mode
  :custom (clojure-toplevel-inside-comment-form t))
#+END_SRC

*** Use Anakondo for static linting
#+begin_src emacs-lisp
(use-package! anakondo
  :hook (clojure-mode . anakondo-minor-mode))
#+END_SRC

*** Lispy config
#+begin_src emacs-lisp
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
#+END_SRC

** Org Config
#+begin_src emacs-lisp
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
#+END_SRC

** Org Use TAB to cycle through visibility states of current subtree
#+begin_src emacs-lisp
(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))
#+END_SRC

** Save frame on quit; restore on load
*** Saves drame dimensions on quit
#+begin_src emacs-lisp :tangle no
(doom-store-put 'last-frame-size
                (list (frame-position)
                      (frame-width)
                      (frame-height)
                      (frame-parameter nil 'fullscreen)))

(defun save-frame-dimensions ()
  (doom-store-put 'last-frame-size
                  (list (frame-position)
                        (frame-width)
                        (frame-height)
                        (frame-parameter nil 'fullscreen))))

(add-hook 'kill-emacs-hook #'save-frame-dimensions)

#+END_SRC
*** Restore frame dimensions on load
#+begin_src emacs-lisp :tangle no
(when-let (dims (doom-store-get 'last-frame-size))
  (when (eq (length dims) 4)
    (cl-destructuring-bind ((left . top) width height fullscreen) dims
      (setq initial-frame-alist
        (append initial-frame-alist
          `((left . ,left)
             (top . ,top)
             (width . ,width)
             (height . ,height)
             (fullscreen . ,fullscreen)))))))
#+END_SRC

** JavaScript
*** Settings
*NOTE:* Relies on editorconfig package
#+begin_src emacs-lisp
(use-package! js2-mode
  :config
  (setq
    js2-basic-offset 2
    js-expr-indent-offset -2
    js-chain-indent nil))
#+END_SRC

*** Indentation
**** Multi-line expressions
#+begin_src emacs-lisp
(defadvice! j/fix-js-multi-line-indent ()
  "Indent expression declarations by 2 just like the rest of the code"
  :after-while #'js--multi-line-declaration-indentation
  (let ((beg (match-beginning 0)))
    (when beg
      (goto-char beg)
      (+ js-indent-level (current-column)))))
#+END_SRC

***** Sample
#+BEGIN_EXAMPLE js
const x = myfunc()
.test
#+END_EXAMPLE
**** TODO regular chaining


** Delete current buffer file
Borrowed from spacemacs
#+begin_src emacs-lisp
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
#+END_SRC

** Rename current buffer file
Borrowed this from spacemacs but uses current name as default
#+begin_src emacs-lisp
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
#+END_SRC

** Copy project path
#+begin_src emacs-lisp
(defun copy-project-path ()
  "Copies the current buffer path from the project root to copy a relative path"
  (interactive)
  (let* ((file-path (buffer-file-name))
         (project-path (or (doom-project-root) ""))
         (rel-path (replace-regexp-in-string (regexp-quote project-path) "" file-path nil 'literal)))
    (kill-new rel-path)))
#+END_SRC

** Zoom font size
Based on the hydra demo zoom example
#+begin_src emacs-lisp
(map!
  :leader
  :desc "Font zoom" "z" #'+hydra/text-zoom/body)
#+END_SRC

** Flycheck
Make the flycheck messages easier to read\distinguish from surrounding code
*** Font customization
#+begin_src emacs-lisp
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
#+END_SRC
*** Format regular flycheck errors
#+begin_src emacs-lisp
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
#+END_SRC
*** Format childframe errors
#+begin_src emacs-lisp
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
#+END_SRC

** Evil Lisp State
*** Install package
#+begin_src emacs-lisp :tangle packages.el
(package! evil-lisp-state)
#+END_SRC
*** Wrap/Unwrap Comment Command
**** Unwrap
#+begin_src emacs-lisp
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
#+END_SRC
**** Wrap
#+begin_src emacs-lisp
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

#+END_SRC
*** Use package
#+begin_src emacs-lisp
(use-package! evil-lisp-state
  :init
  (setq evil-lisp-state-global t)
  :config
  (map!
    :map evil-lisp-state-map
    ";" (evil-lisp-state-enter-command wrap-comment))
  (map! :leader :desc "Lisp" "k" evil-lisp-state-map))
#+END_SRC
*** Modeline
Create a purple state color for the modeline when in lisp-state
#+begin_src emacs-lisp
(after! doom-modeline
  (custom-set-faces!
    '(doom-modeline-evil-operator-state :foreground "#FF9F9E")))
#+end_src

#+begin_src emacs-lisp :tangle no
(defsubst j/doom-modeline--evil ()
  "The current evil state. Requires `evil-mode' to be enabled."
  (when (bound-and-true-p evil-local-mode)
    (doom-modeline--modal-icon
     (let ((tag (evil-state-property evil-state :tag t)))
       (if (stringp tag) tag (funcall tag)))
     (cond
      ((evil-normal-state-p)   'doom-modeline-evil-normal-state)
      ((evil-emacs-state-p)    'doom-modeline-evil-emacs-state)
      ((evil-lisp-state-p)     'doom-modeline-evil-emacs-state)
      ((evil-insert-state-p)   'doom-modeline-evil-insert-state)
      ((evil-motion-state-p)   'doom-modeline-evil-motion-state)
      ((evil-visual-state-p)   'doom-modeline-evil-visual-state)
      ((evil-operator-state-p) 'doom-modeline-evil-operator-state)
      ((evil-vterm-state-p)    'error)
      ((evil-replace-state-p)  'doom-modeline-evil-replace-state)
      (t                       'doom-modeline-evil-normal-state))
     (evil-state-property evil-state :name t))))

(after! (evil-lisp-state doom-modeline)
  (doom-modeline-def-segment modals
    "Displays modal editing states, including `evil', `overwrite', ... etc."
    (let* ((evil (j/doom-modeline--evil))
           (ow (doom-modeline--overwrite))
           (vsep (doom-modeline-vspc))
           (sep (and (or evil ow) (doom-modeline-spc))))
      (concat sep
              (and evil (concat evil (and ow vsep)))
              (and ow (concat ow))
              sep))))
#+END_SRC

Color the cursor in evil-lisp-state
#+begin_src emacs-lisp
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
#+end_src

** Relabel keys in the which-key menu
The which-key menu can sometimes display too long of names which causes them to
be truncated.
#+begin_src emacs-lisp
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
#+END_SRC

** Create new workspace with name prompt, cancel on exit
After creating a workspace prompt to rename. Anon workspaces are not a
fun surprise.

*** Trigger an early load of workspace autoload functions
#+begin_src emacs-lisp
(after! persp-mode
  (+workspace-current-name))
#+END_SRC

*** Advise workspace-new to prompt for name and delete if bad one is generated
#+begin_src emacs-lisp
(defadvice! j/workspace-new (workspace-new &optional _ clone-p)
  "Prompt for workspace name after creating the workspace, delete if no name entered"
  :around #'+workspace/new
  (interactive "iP")
  (let ((name (read-string "Workspace name: "
                           (format "#%s" (+workspace--generate-id)))))
    (when name
      (funcall workspace-new name clone-p))))
#+END_SRC

** Indent guides
*** Show only the active guide
#+begin_src emacs-lisp
(defun j/active-guide (level responsive display)
  (when (eq responsive 'top)
    (highlight-indent-guides--highlighter-default
      level responsive display)))
#+END_SRC

*** Settings
#+begin_src emacs-lisp
(after! highlight-indent-guides
  (setq!
    highlight-indent-guides-auto-enabled         nil
    highlight-indent-guides-responsive           'top
    highlight-indent-guides-delay                0
    highlight-indent-guides-highlighter-function 'j/active-guide)
  (custom-set-faces!
    '(highlight-indent-guides-top-character-face
       :foreground "#DE5356")))
#+END_SRC

*** Hide guides in lisp and clojure modes
#+begin_src emacs-lisp
(add-hook! '(lisp-mode-hook emacs-lisp-mode-hook clojure-mode-hook)
    (defun +disable-indent-guides-in-lisp ()
      (highlight-indent-guides-mode -1)))
#+END_SRC


** Tramp
Set shell to bash for simplicity
#+begin_src emacs-lisp
(after! tramp
  ;; (setenv "SHELL" "/usr/local/bin/fish")
  (setq tramp-default-method "sshx"))
#+END_SRC

** Send to Tmux Session
*** Send text to tmux
#+begin_src emacs-lisp
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
#+END_SRC
*** Select tmux session
**** Get sessions from tmux

#+begin_src emacs-lisp
(defun j/tmux-sessions ()
  "Returns a lit of active tmux-sessions"
  (-> (j/cmd "tmux list-sessions %s %s" "-F" "#S")
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
#+END_SRC

**** API to set a shared target tmux session

#+begin_src emacs-lisp
(defun j/tmux-select-get-session ()
  "Get the tmux session for the given persp or select a new one"
  (interactive)
  (let* ((persp-key (intern (j/persp-name)))
         (session   (plist-get j/tmux-sessions persp-key)))
    (if session
        session
        (j/tmux-select-session))))
#+END_SRC
*** Send Region

#+begin_src emacs-lisp
(defun j/tmux-send-region (beg end &optional append-return)
  "Send region to tmux."
  (interactive "rP")
  (j/tmux-run (buffer-substring-no-properties beg end)
              append-return))
#+END_SRC
*** Send Paragraph

#+begin_src emacs-lisp
(defun j/tmux-send-paragraph ()
  "Send current paragraph to the selected tmux session"
  (interactive)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'paragraph)
    (j/tmux-send-region beg end t)))
#+END_SRC

*** Send org-mode src-block

#+begin_src emacs-lisp
(defun j/tmux-send-src-block ()
  "Send current src block to selected tmux session"
  (interactive)
  (org-babel-when-in-src-block
   (let* ((info (org-babel-get-src-block-info))
          (body (nth 1 info)))
     (j/tmux-run body t))))
#+END_SRC
*** Bindings
#+begin_src emacs-lisp
(after! persp-mode
  (setq j/tmux-sessions '()
        j/tmux-history '())
  (map! :leader
        (:prefix ("e" . "tmux")
         :desc "select-session"      "s" #'j/tmux-select-session
         :desc "tmux-send-region"    "r" #'j/tmux-send-region
         :desc "tmux-send-paragraph" "p" #'j/tmux-send-paragraph
         :desc "tmux-send-src-block" "e" #'j/tmux-send-src-block)))
#+END_SRC


** Navigation in tmux tty

*** Install tmux-pane
#+begin_src emacs-lisp :tangle packages.el
(package! tmux-pane)
#+END_SRC

*** Configuration
#+begin_src emacs-lisp
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
#+END_SRC


** VTerm

*** Key functions
#+begin_src emacs-lisp
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
#+END_SRC

*** Bindings
#+begin_src emacs-lisp
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
#+END_SRC

*** Change the target vterm folder based on the file
#+begin_src emacs-lisp
(defun vterm-buffer-change ()
  (when (derived-mode-p 'vterm-mode)
    (vterm-enter)))

(defadvice! j/vterm-project-root (toggle-vterm arg)
  "Change vterm directory project root"
  :around #'+vterm/toggle
  (let* ((default-directory (or (doom-project-root)
                              default-directory)))
    (funcall toggle-vterm arg)))
#+END_SRC
*** vterm hooks
#+begin_src emacs-lisp
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
#+END_SRC


** Org-Roam
*** Unpin org-roam

#+begin_src emacs-lisp :tangle packages.el
(unpin! org-roam)
#+end_src

*** Set mode hooks, customze directory, bind keys
#+begin_src emacs-lisp
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
#+END_SRC
*** Capture templates
#+begin_src emacs-lisp
(after! org-roam
  (setq! org-roam-capture-templates
   (list
    '("d" "default" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "${dir}%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n"
      :unnarrowed t))))
#+END_SRC

*** Create org-roam files in tag directories
**** Capture
#+begin_src emacs-lisp
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
#+END_SRC


**** Find File
#+begin_src emacs-lisp
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
#+END_SRC

**** Insert
#+begin_src emacs-lisp
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
#+END_SRC

** Org-Roam-Server

*** Install package
#+begin_src emacs-lisp :tangle no
(package! org-roam-server)
#+END_SRC

*** Configuration
Some customizations are coming from Tecosaurs config as well as the
org-roam-server readme

#+begin_src emacs-lisp :tangle no
(after! org-roam-server
  (setq org-roam-server-host "0.0.0.0"
        org-roam-server-port 8989
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files t
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))
#+END_SRC



** Theme preview
Automatically loads the theme as you navigate as =C-spc= doesn't seem to work in
tty mode. There may be a bug in one of the themes that breaks previewing other themes.

#+begin_src emacs-lisp :tangle no
(defadvice! j/counsel-load-theme ()
  "Automatically cycles through themes. Forward to `load-theme'.
Usable with `ivy-resume', `ivy-next-line-and-call' and
`ivy-previous-line-and-call'."
  :override #'counsel-load-theme
  (interactive)
  (let ((current-theme (symbol-name doom-theme)))
    (ivy-read "Load custom theme: "
              (mapcar 'symbol-name
                      (custom-available-themes))
              :preselect current-theme
              :action #'counsel-load-theme-action
              :update-fn (lambda (&rest args)
                           (counsel-load-theme-action (ivy-state-current
                                                       ivy-last)))
              :unwind (lambda (&rest args)
                        (let ((preview-theme (symbol-name doom-theme)))
                          (when (not (equal current-theme preview-theme))
                            (counsel-load-theme-action current-theme))))
              :caller 'counsel-load-theme)))
#+END_SRC



** Improve org-mode formatting
From https://tecosaur.github.io/emacs-config/config.html#org-mode

*** Additional packages
#+begin_src emacs-lisp :tangle packages.el
(package! org-pretty-table-mode
  :recipe (:host github :repo "Fuco1/org-pretty-table"))
(package! org-pretty-tags)
(package! ox-gfm)
(package! org-graph-view
  :recipe (:host github
           :repo "alphapapa/org-graph-view"))
#+END_SRC


*** Configure org-mode
#+begin_src emacs-lisp
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
#+END_SRC

*** Configure org-superstar to change org-mode bullets

#+begin_src emacs-lisp
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
#+END_SRC


*** Disable visual-line-mode and enable auto-fill-mode
:PROPERTIES:
:ID:       78b70e5d-729f-4b1c-bf90-3e603a60cf74
:END:

#+begin_src emacs-lisp
(remove-hook 'text-mode-hook #'visual-line-mode)
#+END_SRC

Add auto-fill-mode to text mode files
#+begin_src emacs-lisp
(add-hook 'text-mode-hook #'auto-fill-mode)
#+END_SRC

*** Spellcheck

#+begin_src emacs-lisp
(after! org (add-hook 'org-mode-hook 'turn-on-flyspell))
#+END_SRC

*** Change enter in org-mode

**** Defines a function to see what we are descendant of, useful for checking context

#+begin_src emacs-lisp
(after! org
  (defun unpackaged/org-element-descendant-of (type element)
    "Return non-nil if ELEMENT is a descendant of TYPE.
TYPE should be an element type, like `item' or `paragraph'.
ELEMENT should be a list like that returned by `org-element-context'."
    ;; MAYBE: Use `org-element-lineage'.
    (when-let* ((parent (org-element-property :parent element)))
      (or (eq type (car parent))
          (unpackaged/org-element-descendant-of type parent)))))
#+END_SRC

**** Do-what-I-mean enter behavior

:PROPERTIES:
:VISIBILITY: folded
:END:
#+begin_src emacs-lisp
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
#+END_SRC

**** Remap the enter key in evil-org-mode-map
#+begin_src emacs-lisp :tangle no
(map!
 :after      evil-org
 :map        evil-org-mode-map
 :i [return] #'unpackaged/org-return-dwim)
#+END_SRC


*** Youtube preview links in exports
#+begin_src emacs-lisp
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
#+END_SRC



*** Make org tables prettier
#+begin_src emacs-lisp
(setq! global-org-pretty-table-mode t)
#+END_SRC



*** Make headings bigger
#+begin_src emacs-lisp
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

#+END_SRC


*** Symbols

See https://tecosaur.github.io/emacs-config/config.html#symbols for more symbols with ligatures enabled

#+begin_src emacs-lisp
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
#+END_SRC



*** Github Flavored Markdown Export
#+begin_src emacs-lisp
(eval-after-load "org"
'(require 'ox-gfm nil t))
#+END_SRC

** Org Mode headers margin bottom

#+begin_src emacs-lisp
(setq!
 org-cycle-separator-lines 1
 org-blank-before-new-entry '((heading .         t)
                              (plain-list-item . auto)))
#+END_SRC

** Zen mode :org:pretty:ui:

Zen mode enables mixed-pitch mode, writeroom mode, and other enhancements
to make writing more enjoyable.

Make sure to enable ~:ui zen~  in your =~/.doom.d/init.el=

#+begin_src emacs-lisp
(add-hook! 'org-mode-hook #'+zen/toggle)
#+END_SRC

If writeroom-mode is not helpful, it can disabled by changing ~:tangle no~ to
~:tangle packages.el~

#+begin_src emacs-lisp :tangle no
(package! writeroom-mode :disable t)
#+end_src


** Highlight fill column
If a line of text exceeds 80 characters in a prog-mode derivative, then
highlight the character at the fill column.

*** Enable Package

#+begin_src emacs-lisp :tangle packages.el
(package! hl-fill-column)
#+END_SRC


*** Configure fill column

#+begin_src emacs-lisp
;;;###autoload
(autoload 'hl-fill-column-mode "hl-fill-column" nil t)

(map! :leader
      (:prefix ("t" . "toggle")
               :desc "fill column" "c" #'hl-fill-column-mode))

(add-hook! 'prog-mode-hook #'hl-fill-column-mode)
#+END_SRC


** Eval typescript in org files

*** Install ob-typescript
#+begin_src emacs-lisp :tangle packages.el
(package! ob-typescript)
#+end_src

*** Configure ob-typescript
#+begin_src emacs-lisp
(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((typescript . t)
     )))
#+END_SRC



** Command log mode :package:debug:

Shows a buffer logging key presses and resulting commands

#+begin_src emacs-lisp :tangle packages.el
(package! command-log-mode)
#+end_src

** Display LSP breadcrumbs automatically :lsp:ui:

Displays a breadcrumbs of project, path, file, and symbols when working in LSP
powered files.

#+begin_src emacs-lisp
(defun j/lsp-breadcrumbs-setup ()
  (setq! lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package! lsp-mode
  :defer t
  :hook (lsp-mode . j/lsp-breadcrumbs-setup))
#+end_src

** Evil move between visual lines instead of logical lines :evil:behavior:remap:

Makes evil jk movements work with soft-wrapped (visual) lines of text. Good for
writing in org and markdown files.

#+begin_src emacs-lisp
(map!
  :after evil
  :nmv [remap evil-previous-line] #'evil-previous-visual-line
  :nmv [remap evil-next-line] #'evil-next-visual-line)
#+end_src

** Change evil surround pairs alist :evil:evil-surround:behavior:

When changing surrounding pairs like from [hello-world] to (hello-world), using
a keyboard shortcut like =cs[(=, it makes more sense to assume it's formatted
correctly. Instead it will add a space after the substitute.

#+begin_src emacs-lisp
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
#+end_src

** Interactive function to convert px to rem :css:conversion:helpers:command:

Prompts for a number like 8 in px then inserts ="0.5rem; //* 8px /*//"=
Intended usage is to press =SPC SPC= in normal mode or =M-x= in insert mode and run
the command directly. It will prompt for the value in px in a minibuffer.

Set a base font-size used for converting to rem. Can be overwritten locally if
desired.

#+begin_src emacs-lisp
(defvar j-css-base-font-size 16.0
  "Used by the px->rem function to convert px units into rem in css. Be sure to
add .0 to get accurate calculations, and this can be overwritten for a specific
buffer or project with dir-locals.el.")
#+end_src

Function to convert px to rem and insert it. If there is an active region, this
function will replace it. If the active region is numeric like 3 or 3px, this
function will use that value.

#+begin_src emacs-lisp
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
#+end_src

Create a binding for it in ~css-mode-map~ localleader =SPC m ?=

#+begin_src emacs-lisp
(map! :localleader
      :map css-mode-map
      :after css-mode
      "j" #'px->rem)
#+end_src

* Distro Configs

** emacs-mac installation config

Add support for the typical cmd-* ops like cmd-s -> save, cmd-v -> paste, cmd-c
-> yank, etc...

#+begin_src emacs-lisp
(map!
 "<H-a>"  #'mark-whole-buffer
 "<H-v>"  #'hydra-paste/evil-paste-after
 "<H-c>"  #'kill-ring-save
 "<H-s>"  #'save-buffer
 "<H-l>"  #'goto-lin
 "<H-w>"  #'delete-window
 "<H-z>"  #'undo)
#+end_src

* Literate Experiments

** Setup

*** Function to execute emacs-lisp src blocks
#+begin_src emacs-lisp :results silent
(defun org-babel-execute:emacs-lisp-config (body params)
  "Execute a block of emacs-lisp code with Babel."
  (cl-letf (((symbol-function 'current-window-configuration) #'ignore)
            ((symbol-function 'set-window-configuration) #'ignore))
    (org-babel-execute:emacs-lisp body params)))
#+END_SRC

*** Added emacs-lisp-config to org-babel using emacs-lisp as base
#+begin_src emacs-lisp :results silent
(after! org
  (add-to-list 'org-src-lang-modes '("emacs-lisp-config" . emacs-lisp)))
#+END_SRC

** Demos
:PROPERTIES:
:header-args: :tangle no
:END:

*** Which Key
#+begin_src emacs-lisp-config
(call-interactively #'helpful-key)
#+END_SRC

*** Splitting windows
#+begin_src emacs-lisp-config
(evil-window-vsplit)

#+END_SRC

#+RESULTS:

* Debugging
#+begin_src emacs-lisp :tangle no
(toggle-debug-on-error)
#+END_SRC

* Local config
    #+INCLUDE: ./local.org
