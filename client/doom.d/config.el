;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jay Zawrotny"
      user-mail-address "jayzawrotny@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq doom-font (font-spec :family "operator mono" :size 14 :weight 'medium))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comment macro
;; - Similar to Clojure's. Lets you wrap any elisp code without eval'ing it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro comment (&rest _)
  `nil)

(comment
 (message "Sup"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Settings
;; - What can I say? I'm fussy.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 tab-always-indent t
 make-backup-files nil
 create-lockfiles nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Bindings
;; - Appease my muscle memory for Spacemacs' window splitting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map! :map evil-window-map
      "/" #'evil-window-vsplit
      "-" #'evil-window-split)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hydra Paste
;; - Create a hydra similar to paste-transient-state to allow me to cycle the
;;   kill ring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(map! :nv "p" #'hydra-paste/evil-paste-after
      :nv "P" #'hydra-paste/evil-paste-before)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! anakondo
  :hook ((clojure-mode . anakondo-minor-mode))
  :commands anakondo-minor-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maximize window size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq initial-frame-alist
  '((fullscreen . maximized)))

;; add to ~/.doom.d/config.el
(when-let (dims (doom-store-get 'last-frame-size))
  (cl-destructuring-bind ((left . top) width height fullscreen) dims
    (setq initial-frame-alist
          (append initial-frame-alist
                  `((left . ,left)
                    (top . ,top)
                    (width . ,width)
                    (height . ,height)
                    (fullscreen . ,fullscreen))))))

(defun save-frame-dimensions ()
  (doom-store-put 'last-frame-size
                  (list (frame-position)
                        (frame-width)
                        (frame-height)
                        (frame-parameter nil 'fullscreen))))

(add-hook 'kill-emacs-hook #'save-frame-dimensions)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript
;; - Fix indent on multi-line exprs
;; - General JS configuration
;; - Relies on editorconfig for most changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun j/fix-js-multi-line-indent ()
  "Indent expression declarations by 2 just like the rest of the code"
  (let ((beg (match-beginning 0)))
    (when beg
      (goto-char beg)
      (+ js-indent-level (current-column)))))

(use-package! js2-mode
  :config
  (setq
   js-expr-indent-offset -2
   js-chain-indent nil)
  (advice-add
   #'js--multi-line-declaration-indentation
   :after-while #'j/fix-js-multi-line-indent))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete Current File
;; - Copies Spacemacs' delete-current-buffer-file to delete the file and buff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zoom
;; - Use the hydra module's zoom example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
  :leader
  :desc "Font zoom" "z" #'+hydra/text-zoom/body)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck Popup Tip Formatting
;; - Flycheck popup is a bit difficult to read\distinguish from surrounding code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun j/format-flycheck-messages (msg)
  (concat
    " "
    flycheck-popup-tip-error-prefix
    (flycheck-error-format-message-and-id msg)
    " "))

(defun j/sort (pred errors)
  (sort errors pred))

(defun j/flycheck-errors->string (errors)
  "Formats ERRORS messages for display. Pads left and right of message with a space"
  (let ((messages (->> errors
                    (delete-dups)
                    (mapcar #'j/format-flycheck-message)
                    (j/sort))))
    (mapconcat 'identity messages "\n")))

(defun j/format-flycheck-popup (errors)
  (-> errors
    (j/flycheck-errors->string)
    (propertize 'face 'popup-tip-face)))

(defun j/flycheck-posframe-format-error (err)
  "Formats ERR for display."
  (propertize (concat
                " "
                (flycheck-posframe-get-prefix-for-error err)
                (flycheck-error-format-message-and-id err)
                " ")
    'face
    `(:inherit ,(flycheck-posframe-get-face-for-error err))) )

(use-package! flycheck
  :config
  (advice-add
    #'flycheck-popup-tip-format-errors
    :override #'j/format-flycheck-popup)
  (comment
    (advice-add
      #'flycheck-posframe-format-error
      :override #'j/flycheck-posframe-format-error)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Insert Newline and continue comments
;; - When pressing o on a repeating comment divider like the line below:
;;   create a new line tht is not in a comment
;; - Consider switching to substring the starter to ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun j/comment-indent (&optional continue)
  "Indent this line's comment to `comment-column', or insert an empty comment.
If CONTINUE is non-nil, use the `comment-continue' markers if any."
  (interactive "*")
  (comment-normalize-vars)
  (let* ((empty (save-excursion (beginning-of-line)
                  (looking-at "[ \t]*$")))
          (starter (or (and continue comment-continue)
                     (and empty block-comment-start) comment-start))
          (ender (or (and continue comment-continue "")
                   (and empty block-comment-end) comment-end)))
    (unless starter (error "No comment syntax defined"))
    (beginning-of-line)
    (let* ((eolpos (line-end-position))
            (begpos (comment-search-forward eolpos t))
            (first-three (substring starter 0 3))
            (fst (substring starter 0 1))
            (test-str (concat fst fst fst))
            cpos indent)
      (if (and comment-insert-comment-function (not begpos))
        ;; If no comment and c-i-c-f is set, let it do everything.
        (funcall comment-insert-comment-function)
        ;; An existing comment?
        (if begpos
          (progn
            (if (and (not (looking-at "[\t\n ]"))
                  (looking-at comment-end-skip))
              ;; The comment is empty and we have skipped all its space
              ;; and landed right before the comment-ender:
              ;; Go back to the middle of the space.
              (forward-char (/ (skip-chars-backward " \t") -2)))
            (setq cpos (point-marker)))
          ;; If none, insert one.
          (save-excursion
              ;; Some `comment-indent-function's insist on not moving
              ;; comments that are in column 0, so we first go to the
              ;; likely target column.
              (indent-to comment-column)
              ;; Ensure there's a space before the comment for things
              ;; like sh where it matters (as well as being neater).
              (unless (memq (char-before) '(nil ?\n ?\t ?\s))
                (insert ?\s))
              (setq begpos (point))
            (unless (equal first-three test-str)
                (insert starter))
            (setq cpos (point-marker))
            (unless (equal first-three test-str)
              (insert ender))))
        (goto-char begpos)
        ;; Compute desired indent.
        (setq indent (save-excursion (funcall comment-indent-function)))
        ;; If `indent' is nil and there's code before the comment, we can't
        ;; use `indent-according-to-mode', so we default to comment-column.
        (unless (or indent (save-excursion (skip-chars-backward " \t") (bolp)))
          (setq indent comment-column))
        ;; (if (not indent)
        ;;   ;; comment-indent-function refuses: delegate to line-indent.
        ;;   (indent-according-to-mode)
        ;;   ;; If the comment is at the right of code, adjust the indentation.
        ;;   (unless (save-excursion (skip-chars-backward " \t") (bolp))
        ;;     (setq indent (comment-choose-indent indent)))
	      ;;   ;; If that's different from comment's current position, change it.
	      ;;   (unless (= (current-column) indent)
	      ;;     (delete-region (point) (progn (skip-chars-backward " \t") (point)))
	      ;;     (indent-to indent)))
	      (goto-char cpos)
	      (set-marker cpos nil)))))

(advice-add #'comment-indent :override #'j/comment-indent)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Lisp State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wrap-comment (&rest args)
  "Wrap sexp in (comment ...) and indent it"
  (interactive "P")
  (sp-wrap-with-pair "(")
  (insert "comment\n")
  (indent-for-tab-command)
  (evil-first-non-blank))

(use-package! evil-lisp-state
  :init
  (setq evil-lisp-state-global t)
  :config
  (map!
    :map evil-lisp-state-map
    ";" (evil-lisp-state-enter-command wrap-comment))
  (map! :leader :desc "Lisp" "k" evil-lisp-state-map))


(defsubst j/doom-modeline--evil ()
  "The current evil state. Requires `evil-mode' to be enabled."
  (when (bound-and-true-p evil-local-mode)
    (doom-modeline--modal-icon
      (let ((tag (evil-state-property evil-state :tag t)))
        (if (stringp tag) tag (funcall tag)))
      (cond
      ((evil-normal-state-p) 'doom-modeline-evil-normal-state)
      ((evil-emacs-state-p) 'doom-modeline-evil-emacs-state)
      ((evil-lisp-state-p) 'doom-modeline-evil-emacs-state)
      ((evil-insert-state-p) 'doom-modeline-evil-insert-state)
      ((evil-motion-state-p) 'doom-modeline-evil-motion-state)
      ((evil-visual-state-p) 'doom-modeline-evil-visual-state)
      ((evil-operator-state-p) 'doom-modeline-evil-operator-state)
      ((evil-replace-state-p) 'doom-modeline-evil-replace-state)
      (t 'doom-modeline-evil-normal-state))
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

(defun j/evil-state-fg (state)
  (let ((sym (intern (concat "doom-modeline-evil-" state "-state"))))
    (face-foreground sym nil t)))

(add-hook! 'doom-load-theme-hook
    (defun j/theme-evil-cursors ()
      (setq
        evil-insert-state-cursor (list 'bar (j/evil-state-fg "insert"))
        evil-normal-state-cursor (list 'box (j/evil-state-fg "normal"))
        evil-lisp-state-cursor   (list 'box (j/evil-state-fg "emacs")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rename Prefixes
;; - The which-key menu can sometimes display too long of names which causes
;;   them to be truncated.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prompt Workspace Rename
;; - After creating a workspace prompt to rename. Anon workspaces are not a
;;   fun surprise.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun j/+workspace/new (&optional name clone-p)
  "Prompt for a workspace name after creating the workspace"
  (interactive "iP")
  (+workspace/new nil clone-p)
  (when-let (newname (read-string
                      "Workspace name: "
                      (+workspace-current-name)))
    (+workspace/rename newname)))

(after! persp-mode
  (+workspace-current-name)
  (map! :leader
    "TAB n" #'j/+workspace/new
    "TAB N" #'+workspace/new))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make line numbers brighter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces!
  '(line-number
    :foreground "#888")
  '(line-number-current-line
    :foreground "#ebbd80"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Indent guides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun j/active-guide (level responsive display)
  (when (eq responsive 'top)
    (highlight-indent-guides--highlighter-default
      level responsive display)))

(after! highlight-indent-guides
  (setq
    highlight-indent-guides-auto-enabled         nil
    highlight-indent-guides-responsive           'top
    highlight-indent-guides-delay                0
    highlight-indent-guides-highlighter-function 'j/active-guide
    highlight-indent-guides-character ?\x2502)
  (custom-set-faces!
    '(highlight-indent-guides-top-character-face
       :foreground "#DE5356")))

;; Uncomment this if you don't like indent guides in lisp languages
(comment
  (add-hook! '(lisp-mode-hook emacs-lisp-mode-hook clojure-mode-hook)
    (defun +disable-indent-guides-in-lisp ()
      (message "Disable indent-guides")
      (highlight-indent-guides-mode -1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Load an optional local file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load! "local.el" nil t)
