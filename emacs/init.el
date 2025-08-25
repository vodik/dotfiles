;;; init.el -*- lexical-binding: t; -*-

(defvar user-emacs-data-directory
  (expand-file-name "emacs/" (or (getenv "XDG_DATA_HOME") "~/.local/share")))
(defvar user-emacs-cache-directory
  (expand-file-name "emacs/" (or (getenv "XDG_CACHE_HOME") "~/.cache")))

(dolist (dir (list user-emacs-data-directory user-emacs-cache-directory))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; Move customize system to a separate file to keep init.el clean
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;;; Set up package system -- straight.el
(let ((bootstrap-version 5)
      (bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-recipes-gnu-elpa-use-mirror t
      straight-vc-git-default-clone-depth 1
      straight-use-package-by-default t)

(require 'package)
(use-package diminish :demand t)
(use-package general :demand t)

;; PROJECT.EL
;; Defined first as workaround for https://github.com/radian-software/straight.el/issues/1146
(use-package project
  :defer t
  :custom
  (project-switch-commands
   '((consult-fd "Find file" ?f)
     (consult-ripgrep "Ripgrep" ?r)
     (project-find-dir "Find dir" ?d)
     (project-dired "Dired" ?D)
     (magit-project-status "Magit" ?g)
     (project-vterm "Vterm" ?v)))
  (project-vc-extra-root-markers '(".project" "Cargo.toml" "package.json" "pyproject.toml" "requirements.txt" "go.mod"))
  :general
  (:states 'normal
   :prefix "\\"
   "pp" 'project-switch-project))

(use-package recentf
  :hook (after-init . recentf-mode))

;; LIGATURES
(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :defer t
  :hook (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures
   'prog-mode
   '("-<<" "-<" "-<-" "<--" "<---" "<<-" "<-" ">>-" ">-" "->-" "-->"
     "--->" "->>" "->" "<->" "<-->" "<--->" "<---->" "<!--" "<!---"
     "=<<" "=<" "=<=" "<==" "<===" "<=" "=>" "=>>" "==>" "===>" "=>="
     ">=" ">==" "<<=" ">>=" "<=>" "<==>" "<===>" "<====>" "[|" "|]"
     "{|" "|}" "</" "</>" "/>" "<>" "<=<" ">=>" "<~~" "<~" "~>" "~~>"
     "::" ":::" "==" "!=" "/=" "~=" "===" "!==" "=/=" "=!=" ":>" "<:"
     "+*" "=*" "*=" "*+" ":=" "<*" "<*>" "*>" "<|" "<|>" "|>" "<." "<.>" ".>"
     "=:" "<***>" "__" "(*" "*)" "/*" "*/" "++" "+++" "|-" "-|"))
  (global-ligature-mode t))

;; EVIL
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-Y-yank-to-eol t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (evil-ex-define-cmd "W[rite]" 'evil-write)
  (evil-ex-define-cmd "Wa[ll]" 'evil-write-all)
  (evil-ex-define-cmd "Q[uit]" 'evil-quit)
  (evil-ex-define-cmd "Wq" 'evil-save-and-close)
  (evil-ex-define-cmd "Quita[ll]" 'evil-quit-all)
  (evil-ex-define-cmd "Qa[ll]" "quitall")
  (evil-ex-define-cmd "bd[elete]" (lambda () (interactive) (kill-buffer)))
  (define-key evil-motion-state-map "\\" nil)
  :general
  (:states 'normal
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line
   "gr" 'xref-find-references
   "gd" 'xref-find-definitions)
  (:states 'motion
   "\\" nil)
  (:states 'visual
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line)
  (:states 'insert
   "C-<return>" 'comment-indent-new-line)
  (:prefix "\\" :states 'normal
   "x" 'delete-trailing-whitespace))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-key-blacklist '("\\" "C-c C-z"))
  :config
  (evil-collection-init))

(use-package evil-textobj-tree-sitter
  :after evil
  :general
  (:keymaps 'evil-outer-text-objects-map
   "f" (evil-textobj-tree-sitter-get-textobj "function.outer")
   "c" (evil-textobj-tree-sitter-get-textobj "class.outer")
   "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))
  (:keymaps 'evil-inner-text-objects-map
   "f" (evil-textobj-tree-sitter-get-textobj "function.inner")
   "c" (evil-textobj-tree-sitter-get-textobj "class.inner")
   "a" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner")))
  (:keymaps 'evil-normal-state-map
   "]f" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer"))
   "]F" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t))
   "[f" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t))
   "[F" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))
   "]c" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer"))
   "]C" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" nil t))
   "[c" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" t))
   "[C" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" t t))))

(use-package evil-commentary
  :diminish
  :after evil
  :config
  (evil-commentary-mode 1))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)
  (push '(?` . ("`" . "'")) evil-surround-pairs-alist)  ; Markdown code
  (push '(?$ . ("${" . "}")) evil-surround-pairs-alist)  ; Template literals
  (push '(?/ . ("/* " . " */")) evil-surround-pairs-alist))  ; Block comments

(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode))

;; ORG MODE
(use-package org
  :defer t
  :mode (("\\.org\\'" . org-mode)
         ("\\.org_archive\\'" . org-mode))
  :hook (org-mode . variable-pitch-mode)
  :commands (org-capture)
  :custom
  (org-tags-column 0)
  (org-log-done 'time)
  (org-deadline-warning-days 14)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation nil)
  (org-src-window-setup 'other-window)
  (org-default-notes-file "~/notes/inbox.org")
  (org-confirm-babel-evaluate nil)
  (org-directory "~/notes")
  (org-odt-preferred-output-format "docx")
  (org-startup-indented t)
  (org-refile-targets '(("~/notes/todo.org" :maxlevel . 3)
                        ("~/notes/tickler.org" :maxlevel . 2)
                        ("~/notes/inbox.org" :maxlevel . 1)
                        ("~/notes/someday.org" :level . 1)))
  (org-capture-templates '(("t" "Todo [inbox]" entry
                            (file+headline "~/notes/inbox.org" "Tasks") "* TODO %i%?")
                           ("T" "Tickler" entry
                            (file+headline "~/notes/tickler.org" "Tickler") "* %i%? \n %^t")))
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path t)
  (org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-todo-keyword-faces
   '(("TODO")
     ("INPROGRESS" :foreground "#0089dd")
     ("WAITING" :foreground "#9f7efe")
     ("DONE")
     ("CANCELLED")))
  (org-agenda-custom-commands '(("@" "Contexts"
                                 ((tags-todo "@email" ((org-agenda-overriding-header "Emails")))
                                  (tags-todo "@phone" ((org-agenda-overriding-header "Phone")))))))
  (org-priority-faces '((?A :foreground "#e45649")
                        (?B :foreground "#da8548")
                        (?C :foreground "#0098dd")))
  :config
  ;; Set fixed-pitch faces for code elements when using variable-pitch-mode
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-meta-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-document-info-keyword nil :inherit 'fixed-pitch)
  (with-eval-after-load 'doom-themes
    (doom-themes-org-config))
  (require 'org-tempo)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t)
                                 (shell . t)
                                 (sql . t)))
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  :general
  ("C-c c" 'org-capture)
  (:keymaps 'org-mode-map
   :states 'normal
   :prefix "\\"
   "ob" 'org-narrow-to-block
   "os" 'org-narrow-to-subtree
   "oe" 'org-narrow-to-element
   "ow" 'widen))

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . (lambda ()
                            (evil-org-set-key-theme))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :general
  (:states 'normal
   "C-o" 'evil-org-org-insert-heading-respect-content-below
   "C-S-o" 'evil-org-org-insert-todo-heading-respect-content-below))

(use-package ox-rst :defer t :after org)
(use-package ox-gfm :defer t :after org)
(use-package ox-typst :straight (:host github :repo "jmpunkt/ox-typst") :defer t :after org)

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '("⁖")))

(use-package org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '((?A . "❗")
                               (?B . "↑")
                               (?C . "↓"))))

;; DENOTE
(use-package denote
  :defer t
  :commands (denote denote-type denote-date denote-subdirectory denote-template
             denote-link denote-find-link denote-backlinks denote-rename-file
             denote-rename-file-using-front-matter)
  :hook ((dired-mode . denote-dired-mode)
         (find-file . denote-rename-buffer-mode))
  :custom
  (denote-directory (expand-file-name "~/notes/"))
  (denote-known-keywords '("emacs" "code" "project" "idea" "reference"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type nil)
  (denote-prompts '(title keywords))
  (denote-date-prompt-use-org-read-date t)
  (denote-rename-buffer-format "[D] %t")
  :config
  (denote-rename-buffer-mode 1)
  :general
  (:states 'normal
   :prefix "\\"
   "nn" 'denote
   "nN" 'denote-type
   "nd" 'denote-date
   "ns" 'denote-subdirectory
   "nt" 'denote-template
   "nl" 'denote-link
   "nL" 'denote-find-link
   "nb" 'denote-backlinks
   "nr" 'denote-rename-file
   "nR" 'denote-rename-file-using-front-matter))

(use-package consult-notes
  :after (denote consult)
  :custom
  (consult-notes-file-dir-sources
   `(("Notes" ?n ,denote-directory)))
  :config
  (consult-notes-denote-mode)
  :general
  (:states 'normal
   :prefix "\\"
   "nf" 'consult-notes
   "nF" 'consult-notes-search-in-all-notes))

;; WHICH KEY
(use-package which-key
  :diminish
  :custom
  (which-key-add-column-padding 1)
  (which-key-idle-delay 0.5)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10)
  (which-key-sort-order #'which-key-prefix-then-key-order)
  (which-key-sort-uppercase-first nil)
  :config
  (which-key-mode 1)
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  ;; Add descriptive labels for leader key mappings
  (which-key-add-key-based-replacements
    "\\" "<leader>"
    "\\c" "code"
    "\\ca" "code actions"
    "\\cc" "claude code"
    "\\cf" "format buffer"
    "\\cr" "rename"
    "\\d" "debug"
    "\\db" "toggle breakpoint"
    "\\dd" "start debugger"
    "\\g" "git dispatch"
    "\\G" "git status"
    "\\l" "list buffers"
    "\\o" "narrow"
    "\\ob" "narrow block"
    "\\oe" "narrow element"
    "\\os" "narrow subtree"
    "\\ow" "widen"
    "\\p" "projects"
    "\\pp" "project list"
    "\\pl" "project buffers"
    "\\q" "AI/GPT"
    "\\qb" "gptel buffer"
    "\\qs" "send to AI"
    "\\qS" "AI menu"
    "\\s" "search"
    "\\sa" "apropos"
    "\\sf" "find files"
    "\\sl" "search lines"
    "\\so" "outline"
    "\\sp" "paste from kill ring"
    "\\sr" "ripgrep"
    "\\ss" "symbols"
    "\\t" "file tree"
    "\\u" "undo tree"
    "\\v" "vterm in project"
    "\\V" "vterm ask directory"
    "\\x" "delete trailing whitespace"
    "\\." "embark act"
    "\\;" "embark dwim"
    "\\/" "search buffer"
    "\\=" "format buffer"))

;; AVY
(use-package avy
  :defer t
  :commands avy-goto-char-timer
  :custom
  (avy-background t)
  :general
  (:states 'normal
   "gs" 'avy-goto-char-timer))

;; SMOOTH SCROLLING
(use-package pixel-scroll
  :straight (:type built-in)
  :custom
  (pixel-scroll-precision-large-scroll-height 40.0)
  (pixel-scroll-precision-use-momentum t)
  :config
  (pixel-scroll-precision-mode t))

;; Themes
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-moonlight t)
  (doom-themes-visual-bell-config))

(use-package catppuccin-theme
  :defer t
  :commands (catppuccin-load-flavor load-theme)
  :custom
  (catppuccin-flavor 'frappe))

(use-package ef-themes
  :defer t
  :commands (ef-themes-select ef-themes-toggle load-theme)
  :custom
  (ef-themes-to-toggle '(ef-dark ef-light))
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t))

(use-package nerd-icons
  :defer t
  :custom
  (nerd-icons-scale-factor 1.0))

(use-package nerd-icons-completion
  :after marginalia nerd-icons
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

(use-package doom-modeline
  :custom
  (doom-modeline-github t)
  (doom-modeline-lsp t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  :hook (after-init . doom-modeline-mode)
  :config
  (set-face-attribute 'mode-line nil :family "Iosevka Aile" :height 100)
  (set-face-attribute 'mode-line-inactive nil :family "Iosevka Aile" :height 100))

(use-package solaire-mode
  :config
  (solaire-global-mode))

(use-package writeroom-mode
  :defer t
  :commands writeroom-mode
  :custom
  (writeroom-width 110))

;; TREE SITTER
(use-package treesit
  :straight (:type built-in)
  :init
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash.git"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c.git"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp.git"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css.git"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile.git"))
          (elixir . ("https://github.com/tree-sitter/tree-sitter-elixir.git"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go.git"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html.git"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript.git"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json.git"))
          (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown.git" "split_parser" "tree-sitter-markdown/src"))
          (markdown_inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown.git" "split_parser" "tree-sitter-markdown-inline/src"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python.git"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby.git"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust.git"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript.git" "master" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript.git" "master" "typescript/src"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
  :custom
  (treesit-font-lock-level 3)
  (treesit-simple-indent-prefer-node-types t)
  :config
  (defun treesit-install-all-language-grammars ()
    "Install all configured tree-sitter language grammars."
    (interactive)
    (dolist (lang treesit-language-source-alist)
      (unless (treesit-language-available-p (car lang))
        (condition-case err
            (treesit-install-language-grammar (car lang))
          (error (message "Failed to install %s grammar: %s" (car lang) err))))))
  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (css-mode . css-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)
          (elixir-mode . elixir-ts-mode)
          (go-mode . go-ts-mode)
          (html-mode . html-ts-mode)
          (js2-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (rust-mode . rust-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (yaml-mode . yaml-ts-mode)))

  (with-eval-after-load 'c-ts-mode
    (setq c-ts-mode-indent-offset 4))

  (with-eval-after-load 'js-ts-mode
    (setq js-indent-level 2
          js-ts-mode-indent-offset 2))

  (with-eval-after-load 'typescript-ts-mode
    (setq typescript-ts-mode-indent-offset 2))

  (with-eval-after-load 'tsx-ts-mode
    (setq tsx-ts-mode-indent-offset 2))

  (with-eval-after-load 'yaml-ts-mode
    (setq yaml-ts-mode-indent-offset 2)))

(add-to-list 'auto-mode-alist '("/\\.envrc.*" . bash-ts-mode))

;; EXPREG
(use-package expreg
  :straight (:host github :repo "casouri/expreg")
  :after treesit
  :general
  (:states '(normal visual)
   "C-SPC" 'expreg-expand
   "C-S-SPC" 'expreg-contract
   "+" 'expreg-expand
   "-" 'expreg-contract))

;; DIRVISH
(use-package dirvish
  :commands (dirvish dirvish-side)
  :custom
  (dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (dirvish-cache-dir (concat user-emacs-cache-directory "/dirvish/"))
  (dirvish-enable-mouse t)
  (dirvish-hide-cursor nil)
  (dirvish-reuse-session t)
  (dirvish-side-attributes '(nerd-icons file-size subtree-state vc-state))
  (dirvish-side-auto-close t)
  (dirvish-side-follow-mode t)
  (dirvish-side-width 35)
  (dirvish-side-window-parameters '((no-other-window . nil)))
  (dirvish-subtree-state-style 'nerd)
  :hook (after-init . dirvish-override-dired-mode)
  :general
  (:states 'normal
   :prefix "\\"
   "t" 'dirvish-side)
  (:keymaps 'dirvish-mode-map
   :states 'normal
   "TAB" 'dirvish-subtree-toggle
   "q" 'dirvish-quit
   [mouse-1] 'dirvish-subtree-toggle))

;; DELIMITERS
(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; VUNDO
(use-package vundo
  :defer t
  :commands vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t)
  :general
  (:prefix "\\" :states 'normal "u" #'vundo)
  (:keymaps 'vundo-mode-map
   "l" #'vundo-forward
   "h" #'vundo-backward
   "j" #'vundo-next
   "k" #'vundo-previous
   "q" #'vundo-quit))

;; BETTER BUFFER NAMES
(use-package uniquify
  :straight (:type built-in)
  :custom
  (uniquify-buffer-name-style 'forward))

;; SPELL CHECKING
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :general
  (:states 'normal
   "z=" #'jinx-correct
   "]s" #'jinx-next
   "[s" #'jinx-previous))

;; GIT
(use-package magit
  :defer t
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-format-file-function #'magit-format-file-nerd-icons)
  (magit-popup-show-common-commands nil)
  :hook (magit-process-mode . goto-address-mode)
  :general
  (:prefix "\\" :states 'normal
   "g" #'magit-dispatch
   "G" #'magit-status))

(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode))

(use-package forge
  :after (magit evil-collection)
  :config
  (evil-collection-forge-setup))

(use-package git-gutter-fringe
  :defer t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package git-gutter
  :defer t
  :hook (prog-mode . git-gutter-mode)
  :config
  (require 'git-gutter-fringe))

(use-package git-modes :defer t)

(use-package gitignore-templates
  :defer t
  :commands (gitignore-templates-insert
             gitignore-templates-new-file))

(use-package xref
  :straight (:type built-in)
  :custom
  (xref-prompt-for-identifier nil))

(use-package indent-bars
  :straight (:host github :repo "jdtsmith/indent-bars")
  :defer t
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-color-by-depth nil)
  (indent-bars-display-on-blank-lines nil)
  (indent-bars-highlight-current-depth nil)
  (indent-bars-pad-frac 0.1)
  (indent-bars-pattern ".")
  (indent-bars-prefer-character t)
  (indent-bars-width-frac 0.1)
  (indent-bars-zigzag nil)
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  :hook ((python-base-mode . indent-bars-mode)
         (yaml-ts-mode . indent-bars-mode)))

;; VERTICO
(use-package vertico
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (vertico-count 20)
  (vertico-cycle t)
  :hook (after-init . vertico-mode)
  :general
  (:keymaps 'vertico-map
   "C-<return>" 'vertico-exit))

;; MARGINALIA
(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :hook (after-init . marginalia-mode)
  :general
  (:keymaps 'minibuffer-local-map
   "M-A" 'marginalia-cycle))

;; ORDERLESS
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((eglot (styles . (orderless flex)))
                                   (file (styles . (partial-completion))))))

;; EMBARK
(use-package embark
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (defun embark-magit-status (file)
    "Run `magit-status` on repo containing the embark target."
    (interactive "GFile: ")
    (magit-status (locate-dominating-file file ".git")))
  :general
  (:keymaps 'vertico-map
   "C-." #'embark-act
   "C-c C-o" #'embark-collect
   "C-c C-e" #'embark-export)
  (:keymaps 'embark-general-map
   "C-g" #'embark-magit-status
   "C-r" #'consult-ripgrep
   "C-f" #'consult-fd)
  (:prefix "\\"
   :states 'normal
   "." #'embark-act
   ";" #'embark-dwim))

;; CONSULT
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  :custom
  (consult-fd-args "fd --hidden --full-path --color=never")
  (consult-ripgrep-args "rg --hidden --glob \!.git --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip")
  (consult-project-function (lambda (_) (when-let* ((proj (project-current)))
                                          (project-root proj))))
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize consult-find consult-fd :state (consult--file-preview))
  :general
  (:prefix "\\"
   :states 'normal
   "l" 'consult-buffer
   "L" 'consult-buffer-other-window
   "pl" 'project-switch-to-buffer
   "/" 'consult-line
   "sa" 'xref-find-apropos
   "sf" 'consult-fd
   "sl" 'consult-line
   "so" 'consult-outline
   "sr" 'consult-ripgrep
   "sp" 'consult-yank-from-kill-ring))

(use-package embark-consult
  :after embark consult
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-eglot
  :after eglot consult
  :general
  (:prefix "\\"
   :states 'normal
   "ss" 'consult-eglot-symbols))

;; EAT
(use-package eat
  :custom
  (eat-kill-buffer-on-exit t)
  :config
  (evil-set-initial-state 'eat-mode 'insert)
  (with-eval-after-load 'which-key
    (which-key-add-keymap-based-replacements eat-mode-map
      "C-c <escape>" "send ESC"
      "C-c C-d" "send C-d"
      "C-c C-c" "send C-c"
      "C-c C-z" "send C-z"))
  :general
  (:keymaps 'eat-mode-map
   "<escape>" 'eat-maybe-send-escape
   "C-c <escape>" (lambda () (interactive) (eat-self-input 1 ?\e))
   "C-c C-d" (lambda () (interactive) (eat-self-input 1 ?\C-d) (evil-insert-state))
   "C-c C-c" (lambda () (interactive) (eat-self-input 1 ?\C-c) (evil-insert-state))
   "C-c C-z" (lambda () (interactive) (eat-self-input 1 ?\C-z) (evil-insert-state))
   "C-c C-t" 'eat-semi-char-mode)
  (:keymaps 'eat-semi-char-mode-map
   "C-c C-t" 'eat-char-mode)
  (:keymaps 'eat-mode-map
   :states 'normal
   "p" 'eat-yank
   "P" 'eat-yank)
  (:keymaps 'eat-mode-map
   :states 'insert
   "C-y" 'eat-yank))

(use-package claude-code-ide
  :straight (:host github :repo "manzaltu/claude-code-ide.el")
  :defer t
  :commands (claude-code-ide-menu claude-code-ide-send-prompt)
  :custom
  (claude-code-ide-terminal-backend 'eat)
  :config
  (claude-code-ide-emacs-tools-setup)
  :general
  (:states 'normal
   :prefix "\\"
   "cc" 'claude-code-ide-menu
   "C" 'claude-code-ide-send-prompt))

;; WGREP
(use-package wgrep :defer t)

;; CORFU
(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :diminish
  :after orderless
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.25)
  (corfu-auto-prefix 1)
  (corfu-count 14)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-cycle t)
  (corfu-on-exact-match 'quit)
  (corfu-preselect 'prompt)
  (corfu-popupinfo-delay 0.5)
  (corfu-popupinfo-max-height 30)
  :hook (after-init . global-corfu-mode)
  :config
  (corfu-popupinfo-mode))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; CAPE
(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :custom
  (cape-dabbrev-min-length 3)
  :general
  (:states 'insert
   "C-x C-f" #'cape-file
   "C-x C-w" #'cape-dabbrev
   "C-x C-:" #'cape-emoji))

;; PRESCIENT
(use-package prescient
  :hook (after-init . prescient-persist-mode)
  :custom
  (prescient-save-file (expand-file-name "prescient-save.el" user-emacs-cache-directory)))

(use-package corfu-prescient
  :after corfu prescient
  :hook (corfu-mode . corfu-prescient-mode)
  :custom
  (corfu-prescient-enable-filtering nil)
  (corfu-prescient-enable-sorting t)
  (corfu-prescient-override-sorting t))

(use-package vertico-prescient
  :after vertico prescient
  :hook (vertico-mode . vertico-prescient-mode)
  :custom
  (vertico-prescient-enable-filtering nil)
  (vertico-prescient-enable-sorting t)
  (vertico-prescient-override-sorting t))

;; ELDOC
(use-package eldoc :straight (:type built-in) :diminish)

(use-package eldoc-box :diminish)

;; PYTHON
(use-package python-ts-mode
  :straight (:type built-in)
  :mode ("\\.py\\'")
  :interpreter ("python" . python-ts-mode)
  :hook ((python-ts-mode . (lambda () (setq-local fill-column 88)))
         (python-ts-mode . (lambda () (add-hook 'before-save-hook #'eglot-format nil t)))))

(use-package cython-mode
  :mode ("\\.pyx\\'"))

;; GO
(use-package go-mode
  :mode ("\\.go\\'"))

;; RUST
(use-package rust-mode
  :mode ("\\.rs\\'")
  :hook ((rust-mode . (lambda () (add-hook 'before-save-hook #'eglot-format nil t)))))

;; HASKELL
(use-package haskell-mode
  :mode ("\\.l?hs\\'"))

;; ELIXIR
(use-package elixir-ts-mode
  :mode ("\\.ex\\'")
  :hook ((elixir-ts-mode . (lambda () (add-hook 'before-save-hook #'eglot-format nil t)))))

;; DOCKER
(use-package dockerfile-mode
  :mode ("Dockerfile")
  :hook ((dockerfile-ts-mode . (lambda () (setq-local tab-width 4) (setq-local standard-indent 4)))
         (dockerfile-ts-mode . (lambda () (add-hook 'before-save-hook #'eglot-format nil t)))))

;; CONFIG
(use-package conf-mode
  :straight (:type built-in)
  :mode (("\\.editorconfig\\'" . conf-mode)
         ("\\.conf\\'" . conf-mode)
         ("\\.cnf\\'" . conf-mode)
         ("\\.cfg\\'" . conf-mode)
         ("\\.env\\'" . conf-mode)
         ("\\.ini\\'" . conf-mode)))

;; PHP
(use-package php-mode
  :mode ("\\.php\\'"))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.blade\\.php\\'" . web-mode))
  :custom
  (web-mode-engines-alist '(("blade" . "\\.blade\\.")))
  (web-mode-markup-indent-offset 2))

;; HY
(use-package hy-mode
  :mode ("\\.hy\\'")
  :interpreter ("hy" . hy-mode))

;; SVELTE
(use-package svelte-mode
  :mode ("\\.svelte\\'"))

;; TYPESCRIPT
(use-package typescript-mode
  :mode ("\\.ts\\'"))

;; TERRAFORM
(use-package terraform-mode
  :mode ("\\.tf\\'")
  :hook (terraform-mode . terraform-format-on-save-mode))

;; LUA
(use-package lua-mode
  :mode ("\\.lua\\'")
  :interpreter ("lua" . lua-mode))

;; WEB APIS
(use-package verb :defer t)

(use-package request :defer t)

(use-package graphql-mode
  :mode ("\\.gql\\'" "\\.graphql\\'"))

;; SNIPPETS
(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :custom
  (yas-snippet-dirs `(,(concat user-emacs-directory "snippets")))
  (yas-triggers-in-field t)
  (yas-wrap-around-region t)
  :config
  (unless (file-exists-p (car yas-snippet-dirs))
    (make-directory (car yas-snippet-dirs) t))
  (yas-reload-all)
  :general
  (:states 'insert
   "C-c y" 'yas-expand
   "C-c n" 'yas-next-field
   "C-c p" 'yas-prev-field))

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

(use-package consult-yasnippet
  :defer t
  :after (consult yasnippet)
  :general
  (:states 'normal
   :prefix "\\"
   "y" 'consult-yasnippet))

;; MISE
(use-package mise
  :hook (after-init . global-mise-mode))

;; LANGUAGE SERVER SUPPORT
(use-package eglot
  :defer t
  :hook (prog-mode . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(elixir-ts-mode "elixir-ls")))
  (setq-default eglot-workspace-configuration
                '(:rust-analyzer (:procMacro (:enable t)
                                  :cargo (:allFeatures t
                                          :buildScripts (:enabled t)
                                          :loadOutDirsFromCheck t)
                                  :checkOnSave (:command "clippy"
                                                :extraArgs ["--", "--all-targets"])
                                  :diagnostics (:experimental (:enable t))
                                  :imports (:granularity (:group "module"))
                                            :prefix "crate")
                  :pylsp (:plugins (:jedi (:enabled t :include_params t :fuzzy t)
                                    :rope (:enabled t)
                                    :ruff (:enabled t :format ["I"])))))
  :general
  (:states 'normal
   :prefix "\\"
   "ca" 'eglot-code-actions
   "cr" 'eglot-rename
   "cf" 'eglot-format-buffer
   "=" 'eglot-format-buffer))

(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(use-package breadcrumb
  :straight (:host github :repo "joaotavora/breadcrumb")
  :diminish
  :defer t
  :hook (prog-mode . breadcrumb-mode))

;; LLM
(use-package gptel
  :defer t
  :commands (gptel gptel-send gptel-menu)
  :custom
  (gptel-model "gpt-4o")
  (gptel-default-mode 'org-mode)
  (gptel-prompt-prefix-alist
   '((markdown-mode . "# ")
     (org-mode . "* ")
     (text-mode . "# ")))
  :config
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(deepseek-r1:7b
              deepseek-r1:14b
              deepseek-r1:32b))
  :general
  (:states 'normal
   :prefix "\\"
   "qb" 'gptel
   "qs" 'gptel-send
   "qS" 'gptel-menu))

;; DAPE
(use-package dape
  :defer t
  :commands (dape dape-breakpoint-toggle)
  :custom
  (dape-buffer-window-arrangement 'right)
  :config
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  (add-hook 'dape-on-stopped-hooks 'dape-repl)
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)
  (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))
  :general
  (:states 'normal
   :prefix "\\"
   "dd" 'dape  ; Start debugger
   "db" 'dape-breakpoint-toggle
   "dB" 'dape-breakpoint-remove-all
   "dc" 'dape-continue
   "dn" 'dape-next
   "ds" 'dape-step-in
   "do" 'dape-step-out
   "dr" 'dape-restart
   "dq" 'dape-quit
   "di" 'dape-info
   "dR" 'dape-repl
   "dw" 'dape-watch-dwim))

;; WHITE SPACE
;; (use-package ws-butler
;;   :diminish
;;   :config
;;   (ws-butler-global-mode t)
;;   (setq ws-butler-keep-whitespace-before-point nil
;;         ws-butler-trim-predicate (lambda (_ end)
;;                                    (not (eq 'font-lock-string-face
;;                                             (get-text-property end 'face))))))

;; WINDOW MANAGEMENT
(use-package winner
  :straight (:type built-in)
  :init (winner-mode)
  :general
  (:keymaps 'evil-window-map "u" 'winner-undo))

(use-package ace-window
  :defer t
  :commands ace-window
  :general
  (:keymaps 'evil-window-map "!" 'ace-window))

;; PKGBUILD
(use-package pkgbuild-mode
  :mode ("PKGBUILD\\'"))

;; MARKDOWN
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)))

;; CONFIG FILES
(use-package yaml-mode :mode ("\\.yml\\'" "\\.yaml\\'"))
(use-package json-mode :mode ("\\.json\\'" "Pipfile\\.lock\\'"))
(use-package toml-mode :mode ("\\.toml\\'" "Pipfile\\'"))

;; JUST
(use-package just-mode)

(use-package justl :defer t)

;; GRPC
(use-package protobuf-mode
  :mode ("\\.proto\\'"))

;; SYSTEMD
(use-package systemd
  :mode ("\\.service\\'"
         "\\.socket\\'"
         "\\.mount\\'"
         "\\.device\\'"))

;; TERMINALS
(use-package vterm
  :custom
  (vterm-always-compile-module t)
  (vterm-buffer-name-string "*vterm[%s]*")
  (vterm-clear-scrollback-when-clearing t)
  (vterm-enable-manipulate-selection-data-by-osc52 t)
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (vterm-shell "zsh")
  (vterm-eval-cmds '(("compile" compile)
                     ("find-file" find-file)
                     ("find-file-other-frame" find-file-other-frame)
                     ("find-file-other-window" find-file-other-window)
                     ("magit-status" magit-status)
                     ("recompile" recompile)
                     ("update-pwd" (lambda (path) (setq default-directory path)))
                     ("vterm-clear-scrollback" vterm-clear-scrollback)))
  :config
  (evil-set-initial-state 'vterm-mode 'insert)
  (with-eval-after-load 'which-key
    (which-key-add-keymap-based-replacements vterm-mode-map
      "C-c <escape>" "send ESC"
      "C-c C-d" "send C-d"
      "C-c C-c" "send C-c"
      "C-c C-z" "send C-z"))

  (defun vterm-display-buffer (buffer-name &optional directory)
    "Create or get a vterm buffer with BUFFER-NAME and display it.
If DIRECTORY is provided, use it as the default-directory."
    (let* ((default-directory (or directory default-directory))
           (buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (unless (eq major-mode 'vterm-mode)
          (vterm-mode)))
      (select-window (display-buffer buffer))))

  (defun project-vterm ()
    "Open vterm in project root."
    (interactive)
    (let* ((project (project-current t))
           (buffer-name (format "*vterm[%s]*" (project-name project))))
      (vterm-display-buffer buffer-name (project-root project))))

  (defun vterm-here (&optional arg)
    "Open vterm in project root or current directory."
    (interactive "P")
    (let* ((directory (or (when-let* ((proj (project-current)))
                           (project-root proj))
                         default-directory))
           (buffer-name (if arg
                           (generate-new-buffer-name "*vterm*")
                         "*vterm*")))
      (vterm-display-buffer buffer-name directory)))

  (defun vterm-ask-directory ()
    "Open vterm in a directory you specify."
    (interactive)
    (let ((directory (read-directory-name "Directory: "))
          (buffer-name (generate-new-buffer-name "*vterm*")))
      (vterm-display-buffer buffer-name directory)))

  :general
  (:keymaps 'vterm-mode-map
   "C-c <escape>" (lambda () (interactive) (vterm-send-key "<escape>"))
   "C-c C-d" (lambda () (interactive) (vterm-send-key "d" nil nil t) (evil-insert-state))
   "C-c C-c" (lambda () (interactive) (vterm-send-key "c" nil nil t) (evil-insert-state))
   "C-c C-z" (lambda () (interactive) (vterm-send-key "z" nil nil t) (evil-insert-state))
   "C-c C-t" 'vterm-copy-mode
   "C-l" 'vterm-clear)
  (:keymaps 'vterm-mode-map
   :states 'normal
   "p" 'vterm-yank
   "P" 'vterm-yank)
  (:keymaps 'vterm-mode-map
   :states 'insert
   "C-y" 'vterm-yank)
  (:keymaps 'vterm-copy-mode-map
   "C-c C-t" 'vterm-copy-mode)
  (:prefix "\\"
   :states 'normal
   "v" 'vterm-here
   "V" 'vterm-ask-directory))

(use-package ansi-color
  :straight (:type built-in)
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

;; HELPFUL
(use-package helpful
  :defer t
  :commands (helpful-callable helpful-variable helpful-key helpful-command helpful-symbol helpful-function)
  :general
  ([remap describe-function] 'helpful-callable
   [remap describe-variable] 'helpful-variable
   [remap describe-key] 'helpful-key
   [remap describe-command] 'helpful-command
   [remap describe-symbol] 'helpful-symbol)
  (:states 'normal
   :prefix "\\"
   "hf" 'helpful-callable
   "hv" 'helpful-variable
   "hk" 'helpful-key
   "hc" 'helpful-command
   "hs" 'helpful-symbol
   "hF" 'helpful-function))

;; LINE NUMBERS
(use-package display-line-numbers
  :straight (:type built-in)
  :custom
  (display-line-numbers-type 'relative)
  :hook
  (prog-mode . display-line-numbers-mode))

;; LINE HIGHLIGHTING
(use-package hl-line
  :straight (:type built-in)
  :hook
  ((text-mode prog-mode) . hl-line-mode))

;; FLYMAKE
(use-package flymake
  :straight (:type built-in)
  :custom
  (flymake-show-diagnostics-at-end-of-line 'fancy))

(use-package autorevert
  :straight (:type built-in)
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode))

(use-package savehist
  :straight (:type built-in)
  :hook (after-init . savehist-mode)
  :custom
  (savehist-file (expand-file-name "savehist" user-emacs-cache-directory)))

(use-package emacs
  :straight (:type built-in)
  :hook
  ((after-init . (lambda ()
                   (set-face-attribute 'default nil :family "Iosevka SS10 Extended" :height 100)
                   (set-face-attribute 'fixed-pitch nil :family "Iosevka SS10 Extended" :height 100)
                   (set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 100)))
   (text-mode . turn-on-visual-line-mode))
  :custom
  (ring-bell-function 'ignore)
  (indent-tabs-mode nil)
  (tab-always-indent 'complete)
  (use-default-font-for-symbols nil)

  (user-full-name "Simon Gomizelj")
  (user-mail-address "simon@vodik.xyz")

  (compilation-scroll-output 'first-error)
  (comint-move-point-for-output t)
  (comint-input-ring-size 5000)
  (eshell-scroll-to-bottom-on-output t)

  (window-sides-vertical t)

  (display-buffer-base-action
   '((display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-use-some-window)
     (reusable-frames . nil)
     (inhibit-same-window . nil)))

  (display-buffer-alist
   '(((or (major-mode . vterm-mode)
          (major-mode . eat-mode)
          (major-mode . eshell-mode)
          (major-mode . shell-mode)
          (major-mode . term-mode))
      (display-buffer-reuse-mode-window display-buffer-in-side-window)
      (window-height . 0.20)
      (dedicated . t))

     ((or (major-mode . compilation-mode)
          (major-mode . grep-mode)
          (major-mode . occur-mode)
          (major-mode . embark-collect-mode))
      (display-buffer-reuse-mode-window display-buffer-at-bottom)
      (window-height . 0.30)
      (mode . (compilation-mode grep-mode occur-mode embark-collect-mode)))

     ((or (major-mode . help-mode)
          (major-mode . helpful-mode)
          (major-mode . Man-mode)
          "\\*[Ee]ldoc.*\\*")
      (display-buffer-in-side-window)
      (side . right)
      (window-width . 0.25))))
  :config
  (show-paren-mode 1)

  (pcase system-type
    ('darwin
     (setq-default browse-url-browser-function 'browse-url-firefox
                   browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox"))
    ('gnu/linux
     (setq-default browse-url-browser-function 'browse-url-xdg-open)))

  (let* ((backup-dir (concat user-emacs-data-directory "/backup")))
    (unless (file-directory-p backup-dir)
      (mkdir backup-dir t))
    (setq-default backup-directory-alist `(("." . ,backup-dir))))
    
  (let* ((auto-save-dir (concat user-emacs-cache-directory "/auto-save/")))
    (unless (file-directory-p auto-save-dir)
      (mkdir auto-save-dir t))
    (setq auto-save-file-name-transforms
          `((".*" ,auto-save-dir t)))
    (setq auto-save-list-file-prefix
          (concat auto-save-dir "session.")))
    
  (let* ((lock-file-dir (concat user-emacs-cache-directory "/lock-files/")))
    (unless (file-directory-p lock-file-dir)
      (mkdir lock-file-dir t))
    (setq lock-file-name-transforms
          `((".*" ,lock-file-dir t))))

  (defun sort-lines-nocase ()
    "Sort lines case-insensitively."
    (interactive)
    (let ((sort-fold-case t))
      (call-interactively 'sort-lines))))

;;; init.el ends here
