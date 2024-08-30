;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Rigoberto L. Perez"
      user-mail-address "rlperez@kablamo.me")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
(setq doom-font (font-spec :family "Hasklug Nerd Font" :size 18)
      doom-serif-font (font-spec :family "NotoSerif Nerd Font" :size 18)
      doom-big-font (font-spec :family "Hasklug Nerd Font" :size 24)
      doom-variable-pitch-font (font-spec :family "Latin Modern Sans" :size 16))
(setq kill-whole-line t)
(global-auto-revert-mode 1)
(after! flycheck (setq flycheck-checker-error-threshold 250))
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-feather-dark)

;; TODO: Figure out what part of this is not working
;;(let ((alternatives (concat (mapcar (lambda (x) (concat "splashes/doom/" x))
;;                                    (nthcdr 2 (directory-files (concat doom-user-dir ".doom.d/splashes/doom"))))
;;                            (mapcar (lambda (x) (concat "splashes/emacs/" x))
;;                                    (nthcdr 2 (directory-files (concat doom-user-dir ".doom.d/splashes/emacs"))))
;;                            (mapcar (lambda (x) (concat "splashes/gnu/" x))
;;                                    (nthcdr 2 (directory-files (concat doom-user-dir ".doom.d/splashes/gnu"))))
;;                            (mapcar (lambda (x) (concat "splashes/others/" x))
;;                                    (nthcdr 2 (directory-files (concat doom-user-dir ".doom.d/splashes/others")))))))
;;  (setopt fancy-splash-image
;;          (concat doom-user-dir ".doom.d/"
;;                  (nth (random (length alternatives)) alternatives))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-ine-numbers-type 'nil)
(display-line-numbers-mode 0)
;; (line-number-mode)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(use-package! treesit-auto
  :defer t
  :after tree-sitter
  :config
  (global-treesit-auto-mode))
(use-package! eglot-booster
  :defer t
  :after eglot
  :config (eglot-booster-mode))

;; A Flymake backend for Javascript using eslint
;; https://github.com/orzechowskid/flymake-eslint/issues/23#issuecomment-1675481378
(use-package! flymake-eslint
  :hook
  (eglot-managed-mode . (lambda ()
                          (when (and (derived-mode-p 'typescript-ts-mode 'typescript-mode 'web-mode 'js-mode)
                                     (executable-find "eslint"))
                            (flymake-eslint-enable)))))

(use-package! kkp
  :defer t
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1))

(use-package! clipetty
  :defer t
  :hook
  (after-init . global-clipetty-mode))

(setopt auto-revert-avoid-polling t)
;; Fix archaic defaults
(setopt sentence-end-double-space nil
        require-final-newline t)

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion
(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates
(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager
(setopt completion-auto-select t)                      ; See `C-h v completion-auto-select' for more possible values
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well
(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent
(setopt show-trailing-whitespace t)
(setopt evil-want-minibuffer t)
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;;; Fix all of the things wrong with debug configurations...how the people that write this
;;; have left it in such a retarded non functional state
(setopt dap-auto-configure-mode t)
(after! dap-mode
  (require 'dap-cpptools)
  (require 'dap-python)
  (require 'dap-ruby)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (require 'dap-dlv-go)
  (require 'dap-go)
  (require 'dap-firefox)
  (require 'dap-chrome)
  (require 'dap-edge)
  (require 'dap-node)
  (require 'dap-netcore)
  (setopt dap-auto-configure-mode t)
  (setopt dap-python-debugger 'debugpy)
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :gdbpath "rust-gdb"
                                     :target nil
                                     :cwd nil))
  (dap-register-debug-template "Kotlin tests with launcher"
                               (list :type "kotlin"
                                     :request "launch"
                                     :mainClass "org.junit.platform.console.ConsoleLauncher --scan-class-path"
                                     :enableJsonLogging nil
                                     :noDebug nil)))

(after! eglot
  (setopt eglot-events-buffer-size 0
          eglot-ignored-server-capabilities '(:inlayHintProvider)
          eglot-confirm-server-initiated-edits nil))
(after! rustic
  (setopt rustic-cargo-bin-remote "/usr/local/cargo/bin/cargo"
          rustic-lsp-client 'eglot))
(after! vterm
  (add-hook
   'vterm-mode-hook
   (lambda() (setq-local show-trailing-whitespace nil)))
  (setopt vterm-shell "/bin/fish"))
(after! neil
  (setopt neil-prompt-for-version-p nil
          neil-inject-dep-to-project-p t))
(after! web-mode
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . clojurescript-mode)))

(after! cider-mode
  (setopt cider-repl-buffer-size-limit 100000)
  (add-hook 'cider-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'cider-load-buffer
                        nil
                        'make-it-local))))

;; TODO: Delete if this is otherwise working. Could be useful later though...
;; (after! cider-mode
;;   (add-hook 'cider-mode-hook
;;             (lambda ()
;;               (defun cider-after-save-restart-hook ()
;;                 (cider-load-buffer)
;;                 (let ((current-namespace (cider-current-ns))
;;                       (prefixes '("thristysink"))) ; Your namespace prefixes
;;                   (dolist (prefix prefixes)
;;                     (when (string-prefix-p prefix current-namespace)
;;                       ;; Construct the command using the prefix
;;                       (let ((command (format "(%s.core/restart)\n" prefix)))
;;                         ;; Send the command to the REPL
;;                         (cider-repl--input-sender command)))))
;;                 ))
;;             (add-hook 'after-save-hook 'cider-after-save-restart-hook nil 'local)))

(menu-bar-mode 1)
(tool-bar-mode 0)

;; A Flymake backend for Javascript using eslint
;; https://github.com/orzechowskid/flymake-eslint/issues/23#issuecomment-1675481378
;; (use-package flymake-eslint
;;  :hook
;;  (eglot-managed-mode . (lambda ()
;;                          (when (and (derived-mode-p 'typescript-ts-mode 'typescript-mode 'web-mode 'js-mode)
;;                                     (executable-find "eslint"))
;;                            (flymake-eslint-enable)))))

;; Fix the retarded ass default behavior of project.el
(defcustom project-root-markers
  '("Cargo.toml" "compile_commands.json" "compile_flags.txt"
    "project.clj" ".git" "deps.edn" "shadow-cljs.edn" "package.json"
    "pom.xml" "build.xml" "build.gradle.kts" "build.gradle" "go.mod"
    ".project.el" "project.el")
  "Files or directories that indicate the root of a project."
  :type '(repeat string)
  :group 'project)

(defun project-root-p (path)
  "Check if the current PATH has any of the project root markers."
  (catch 'found
    (dolist (marker project-root-markers)
      (when (file-exists-p (concat path marker))
        (throw 'found marker)))))

(defun project-find-root (path)
  "Search up the PATH for `project-root-markers'."
  (when-let ((root (locate-dominating-file path #'project-root-p)))
    (cons 'transient (expand-file-name root))))

(defun project-save-some-buffers (&optional arg)
  "Save some modified file-visiting buffers in the current project.
Optional argument ARG (interactively, prefix argument) non-nil
means save all with no questions."
  (let* ((project-buffers (project-buffers (project-current)))
         (pred (lambda () (memq (current-buffer) project-buffers))))
    (funcall-interactively #'save-some-buffers arg pred)))

(define-advice project-compile (:around (fn) save-project-buffers)
  "Only ask to save project-related buffers."
  (let* ((project-buffers (project-buffers (project-current)))
         (compilation-save-buffers-predicate
          (lambda () (memq (current-buffer) project-buffers))))
    (funcall fn)))

(define-advice recompile (:around (fn &optional edit-command) save-project-buffers)
  "Only ask to save project-related buffers if inside a project."
  (if (project-current)
      (let* ((project-buffers (project-buffers (project-current)))
             (compilation-save-buffers-predicate
              (lambda () (memq (current-buffer) project-buffers))))
        (funcall fn edit-command))
    (funcall fn edit-command)))
