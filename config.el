;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")
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
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (font-spec :family "JetBrains Mono Nerd Font" :size 18)
      ;; (font-spec :family "Hack Nerd Font" :size 18)
      ;; (font-spec :family "Hasklug Nerd Font" :size 18)
      doom-serif-font (font-spec :family "NotoSerif Nerd Font" :size 18)
      doom-big-font (font-spec :family "Hasklug Nerd Font" :size 24)
      doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 16))

(setq kill-whole-line t)
(global-auto-revert-mode 1)
(after! flycheck (setq flycheck-checker-error-threshold 250))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)
;;(setq doom-theme 'doom-feather-dark)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(display-line-numbers-mode 0)

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

(use-package! flymake-eslint
  :hook
  (eglot-managed-mode . (lambda ()
                          (when (and (derived-mode-p 'typescript-ts-mode 'typescript-mode 'web-mode 'js-mode)
                                     (or (file-exists-p ".eslintrc.js")
                                         (file-exists-p ".eslint.ts")
                                         (file-exists-p ".eslintrc.mjs")
                                         (file-exists-p ".eslintrc.mts")
                                         (file-exists-p ".eslintrc.cts")
                                         (file-exists-p ".eslintrc.cjs"))
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

(use-package! org-super-agenda
  :defer t)

(use-package! comment-tags
  :defer t)

(setopt auto-revert-avoid-polling t)
;; Fix archaic defaults
(setopt sentence-end-double-space nil
        require-final-newline t)

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
  (setopt rustic-cargo-bin-remote "/home/fr0bar/.cargo/bin"
          rustic-lsp-client 'eglot))

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

(after! vterm
  (add-hook
   'vterm-mode-hook
   (lambda() (setq-local show-trailing-whitespace t))))

(after! emacs-eat
  (setq eat-shell "fish")
  (setq explicit-shell-file-name "/usr/bin/fish")
  (add-hook 'eshell-first-time-mode-hook
            #'eat-eshell-visual-command-mode)
  (add-hook 'eshell-first-time-mode-hook #'eat-shell-mode)
  (add-hook 'eat-mode-hook
            (lambda ()
              (setq-local explicit-shell-file-name "/usr/bin/fish"))))

(after! web-mode
  (add-to-list 'auto-mode-alist '("\\.liquid\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.htm??\\'" . web-mode)))

(after! just-mode
  (add-to-list 'auto-mode-alist '("\\.just\\'" . just-mode))
  (add-to-list 'auto-mode-alist '("\\justfile\\(\\.\\)?.+" . just-mode)))

(after! cider-mode
  (setopt cider-repl-buffer-size-limit 100000)
  (add-hook 'cider-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'cider-load-buffer
                        nil
                        'make-it-local))))

(setopt show-trailing-whitespace t)

(after! eglot
  (setopt eglot-events-buffer-size 0
          eglot-ignored-server-capabilities '(:inlayHintProvider)
          eglot-confirm-server-initiated-edits nil))

(after! vterm
  (add-hook
   'vterm-mode-hook
   (lambda() (setq-local show-trailing-whitespace nil)))
  (setopt vterm-shell "/bin/fish"))

(after! mise
  (add-hook 'after-init-hook #'global-mise-mode))

(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell (executable-find "fish"))
(setq-default explicit-shell-file-name (executable-find "fish"))

(menu-bar-mode 1)
(tool-bar-mode 0)

;; Fix the retarded ass default behavior of project.el
(defcustom project-root-markers
  '("Cargo.toml" "compile_commands.json" "compile_flags.txt"
    "project.clj" ".git" "deps.edn" "shadow-cljs.edn" "package.json"
    "pom.xml" "build.xml" "build.gradle.kts" "build.gradle" "go.mod"
    "*.sln" "*.csproj" "*.fsproj" ".project.el" "project.el")
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

;; Function to setup mise environment for the current project
(defun setup-mise-environment-for-project (project)
  "Set up the environment for the PROJECT."
  (when project
    (let ((project-root (cdr project)))
      (with-current-buffer (find-file-noselect project-root)
        (call-process-shell-command "mise shell" nil nil nil)))))

;; Modify project-find-functions to run mise for recognized projects
(add-hook 'project-find-functions (lambda (dir)
                                    (when-let ((project (project-find-root dir)))
                                      (setup-mise-environment-for-project project)
                                      project)))

(setq org-log-done 'time)
(setq org-return-follows-link  t)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-hide-emphasis-markers t)

;; TODO states
(setq org-todo-keywords
      '((sequence "TODO(t)" "PLANNING(p)" "IN-PROGRESS(i@/!)" "VERIFYING(v!)" "BLOCKED(b@)"  "|" "DONE(d!)" "OBE(o@!)" "WONT-DO(w@/!)" )))

;; TODO colors
(setq org-todo-keyword-faces
      '(
        ("TODO" . (:foreground "GoldenRod" :weight bold))
        ("PLANNING" . (:foreground "DeepPink" :weight bold))
        ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
        ("VERIFYING" . (:foreground "DarkOrange" :weight bold))
        ("BLOCKED" . (:foreground "Red" :weight bold))
        ("DONE" . (:foreground "LimeGreen" :weight bold))
        ("OBE" . (:foreground "LimeGreen" :weight bold))
        ("WONT-DO" . (:foreground "LimeGreen" :weight bold))))

(setq org-tag-alist '(
                      ;; Ticket types
                      (:startgroup . nil)
                      ("@bug" . ?b)
                      ("@feature" . ?u)
                      ("@spike" . ?j)
                      (:endgroup . nil)

                      ;; Ticket flags
                      ("@write_future_ticket" . ?w)
                      ("@emergency" . ?e)
                      ("@research" . ?r)

                      ;; Meeting types
                      (:startgroup . nil)
                      ("big_sprint_review" . ?i)
                      ("cents_sprint_retro" . ?n)
                      ("dsu" . ?d)
                      ("grooming" . ?g)
                      ("sprint_retro" . ?s)
                      (:endgroup . nil)

                      ;; Code TODOs tags
                      ("QA" . ?q)
                      ("backend" . ?k)
                      ("broken_code" . ?c)
                      ("frontend" . ?f)
                      ("feature" . ?F)
                      ("bug" . ?b)
                      ("refactor" . ?r)

                      ("pgmq" . ?P)
                      ("kablamo.me" . ?K)
                      ("thirstysink" . ?T)

                      ;; Special tags
                      ("CRITICAL" . ?x)
                      ("obstacle" . ?o)

                      ;; Meeting tags
                      ("HR" . ?h)
                      ("general" . ?l)
                      ("meeting" . ?m)
                      ("misc" . ?z)
                      ("planning" . ?p)

                      ;; Work Log Tags
                      ("accomplishment" . ?a)))

;; Agenda View "d"
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

  PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(setq org-agenda-skip-deadline-if-done t)

(setq org-agenda-custom-commands
      '(
        ;; James's Super View
        ("j" "James's Super View"
         (
          (agenda ""
                  (
                   (org-agenda-remove-tags t)
                   (org-agenda-span 7)
                   )
                  )

          (alltodo ""
                   (
                    ;; Remove tags to make the view cleaner
                    (org-agenda-remove-tags t)
                    (org-agenda-prefix-format "  %t  %s")
                    (org-agenda-overriding-header "CURRENT STATUS")

                    ;; Define the super agenda groups (sorts by order)
                    (org-super-agenda-groups
                     '(
                       ;; Filter where tag is CRITICAL
                       (:name "Critical Tasks"
                        :tag "CRITICAL"
                        :order 0
                        )
                       ;; Filter where TODO state is IN-PROGRESS
                       (:name "Currently Working"
                        :todo "IN-PROGRESS"
                        :order 1
                        )
                       ;; Filter where TODO state is PLANNING
                       (:name "Planning Next Steps"
                        :todo "PLANNING"
                        :order 2
                        )
                       ;; Filter where TODO state is BLOCKED or where the tag is obstacle
                       (:name "Problems & Blockers"
                        :todo "BLOCKED"
                        :tag "obstacle"
                        :order 3
                        )
                       ;; Filter where tag is @write_future_ticket
                       (:name "Tickets to Create"
                        :tag "@write_future_ticket"
                        :order 4
                        )
                       ;; Filter where tag is @research
                       (:name "Research Required"
                        :tag "@research"
                        :order 7
                        )
                       ;; Filter where tag is meeting and priority is A (only want TODOs from meetings)
                       (:name "Meeting Action Items"
                        :and (:tag "meeting" :priority "A")
                        :order 8
                        )
                       ;; Filter where state is TODO and the priority is A and the tag is not meeting
                       (:name "Other Important Items"
                        :and (:todo "TODO" :priority "A" :not (:tag "meeting"))
                        :order 9
                        )
                       ;; Filter where state is TODO and priority is B
                       (:name "General Backlog"
                        :and (:todo "TODO" :priority "B")
                        :order 10
                        )
                       ;; Filter where the priority is C or less (supports future lower priorities)
                       (:name "Non Critical"
                        :priority<= "C"
                        :order 11
                        )
                       ;; Filter where TODO state is VERIFYING
                       (:name "Currently Being Verified"
                        :todo "VERIFYING"
                        :order 20
                        )))))))))

;; Tag colors
(setq org-tag-faces
      '(
        ("planning"    . (:foreground "mediumPurple1" :weight bold))
        ("backend"     . (:foreground "royalblue1"    :weight bold))
        ("frontend"    . (:foreground "forest green"  :weight bold))
        ("feature"     . (:foreground "white"         :weight bold :background "black"))
        ("bug"         . (:foreground "white"         :weight bold :background "maroon"))
        ("refactor"    . (:foreground "white"         :weight bold :background "forest green"))
        ("kablamo.me"  . (:foreground "orange"        :weight bold))
        ("thirstysink" . (:foreground "teal"          :weight bold))
        ("pgmq"        . (:foreground "dark-blue"     :weight bold))
        ("QA"          . (:foreground "sienna"        :weight bold))
        ("meeting"     . (:foreground "yellow1"       :weight bold))
        ("CRITICAL"    . (:foreground "red1"          :weight bold))))

(setq org-capture-templates
      '(
        ("j" "Work Log Entry"
         entry (file+datetree "~/org/work-log.org")
         "* %?"
         :empty-lines 0)
        ("n" "Note"
         entry (file+headline "~/org/notes.org" "Random Notes")
         "** %?"
         :empty-lines 0)
        ("g" "General To-Do"
         entry (file+headline "~/org/todos.org" "General Tasks")
         "* TODO [#B] %?\n:Created: %T\n "
         :empty-lines 0)
	("c" "Code To-Do"
         entry (file+headline "~/org/todos.org" "Code Related Tasks")
         "* TODO [#B] %?\n:Created: %T\n%i\n%a\nProposed Solution: "
         :empty-lines 0)
	("m" "Meeting"
         entry (file+datetree "~/org/meetings.org")
         "* %? :meeting:%^g \n:Created: %T\n** Attendees\n*** \n** Notes\n** Action Items\n*** TODO [#A] "
         :tree-type week
         :clock-in t
         :clock-resume t
         :empty-lines 0)))
