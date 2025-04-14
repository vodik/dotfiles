;;; init.el -*- lexical-binding: t; -*-

(setq-default user-emacs-data-directory
              (concat (getenv "HOME") "/.local/share/emacs"))
(setq-default user-emacs-cache-directory
              (concat (getenv "HOME") "/.cache/emacs"))

;;; Set up package system -- straight.el
(defvar bootstrap-version)
(or (boundp 'native-comp-deferred-compilation-deny-list)
    (setq native-comp-deferred-compilation-deny-list '()))

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq warning-minimum-level :error)
(setq native-comp-async-report-warnings-errors nil)

(setq straight-vc-git-default-clone-depth 1)

;; Install use-package
(straight-use-package 'use-package)
(use-package diminish :straight t)
(use-package general :straight t)

;; Workaround for https://github.com/radian-software/straight.el/issues/1146
(use-package project :straight t)
(use-package pcre2el :straight t)

(use-package recentf
  :config (recentf-mode 1))

;; LIGATURES
(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
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
  :straight t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-Y-yank-to-eol t
        evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (evil-ex-define-cmd "W[rite]" 'evil-write)
  (evil-ex-define-cmd "Wa[ll]" 'evil-write-all)
  (evil-ex-define-cmd "Q[uit]" 'evil-quit)
  (evil-ex-define-cmd "Wq" 'evil-save-and-close)
  (evil-ex-define-cmd "Quita[ll]" 'evil-quit-all)
  (evil-ex-define-cmd "Qa[ll]" "quitall")
  (evil-ex-define-cmd "Qa[ll]" "quitall")
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
  (:prefix "\\" :states 'normal
   "x" 'delete-trailing-whitespace))

(use-package evil-collection
  :straight t
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))

(use-package evil-textobj-tree-sitter
  :straight t
  :after evil
  :config
  (defun my--goto-next-textobj (textobj &optional end)
    (lambda ()
      (interactive)
      (evil-textobj-tree-sitter-goto-textobj textobj nil end)))
  (defun my--goto-prev-textobj (textobj &optional end)
    (lambda ()
      (interactive)
      (evil-textobj-tree-sitter-goto-textobj textobj t end)))
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
   "]f" (my--goto-next-textobj "function.outer")
   "]F" (my--goto-next-textobj "function.outer" t)
   "[f" (my--goto-prev-textobj "function.outer")
   "[F" (my--goto-prev-textobj "function.outer" t)
   "]c" (my--goto-next-textobj "class.outer")
   "]C" (my--goto-next-textobj "class.outer" t)
   "[c" (my--goto-prev-textobj "class.outer")
   "[C" (my--goto-prev-textobj "class.outer" t)))

(use-package evil-commentary
  :straight t
  :diminish
  :init (evil-commentary-mode))

(use-package evil-matchit
  :straight t
  :init (global-evil-matchit-mode 1))

(use-package evil-goggles
  :straight t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; WHICH KEY
(use-package which-key
  :straight t
  :diminish
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (which-key-add-key-based-replacements "\\" "<leader>")
  (which-key-mode +1))

;; AVY
(use-package avy
  :straight t
  :config
  (setq avy-background t)
  :general
  (:states 'normal
   "gs" 'avy-goto-char-timer))

;; SMOOTH SCROLLING
(setq pixel-scroll-precision-large-scroll-height 40.0)
(pixel-scroll-precision-mode t)

;; VISUALS
(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-treemacs-enable-variable-pitch nil)
  (load-theme 'doom-palenight t)
  (doom-themes-visual-bell-config))

(use-package all-the-icons
  :straight t
  :config (setq all-the-icons-scale-factor 1.0))

(use-package all-the-icons-completion
  :straight t
  :after marginalia all-the-icons
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package doom-modeline
  :straight t
  :init
  (doom-modeline-mode 1)
  :config
  (set-face-attribute 'mode-line nil :family "Iosevka Aile" :height 100)
  (set-face-attribute 'mode-line-inactive nil :family "Iosevka Aile" :height 100)
  (setq doom-modeline-github t
        doom-modeline-lsp t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t))

(use-package solaire-mode
  :straight t
  :hook (after-init . solaire-global-mode))

(use-package writeroom-mode
  :straight t
  :config
  (setq writerroom-width 110))

;; TREE SITTER
(pcase system-type
  ('darwin
   (use-package treesit
     :ensure nil
     :init
     (setq treesit-language-source-alist
           '((bash . ("https://github.com/tree-sitter/tree-sitter-bash.git"))
             (css . ("https://github.com/tree-sitter/tree-sitter-css.git"))
             (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile.git"))
             (elixir . ("https://github.com/tree-sitter/tree-sitter-elixir.git"))
             (html . ("https://github.com/tree-sitter/tree-sitter-html.git"))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript.git"))
             (json . ("https://github.com/tree-sitter/tree-sitter-json.git"))
             (python . ("https://github.com/tree-sitter/tree-sitter-python.git"))
             (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby.git"))
             (rust . ("https://github.com/tree-sitter/tree-sitter-rust.git"))
             (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript.git" "master" "typescript/src"))
             (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
     :config
     (defun treesit-install-all-language-grammars ()
       "Install all tree-sitter languages if not already present."
       (interactive)
       (dolist (lang treesit-language-source-alist)
         (unless (treesit-language-available-p (car lang))
           (treesit-install-language-grammar (car lang)))))

     (setq major-mode-remap-alist
           '((bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (dockerfile-mode . dockerfile-ts-mode)
             (elixir-mode . elixir-ts-mode)
             (html-mode . html-ts-mode)
             (js2-mode . js-ts-mode)
             (json-mode . json-ts-mode)
             (python-mode . python-ts-mode)
             (ruby-mode . ruby-ts-mode)
             (rust-mode . rust-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (yaml-mode . yaml-ts-mode)))))
  (_
   (setq major-mode-remap-alist
         '((bash-mode . bash-ts-mode)
           (css-mode . css-ts-mode)
           (elixir-mode . elixir-ts-mode)
           (js2-mode . js-ts-mode)
           (json-mode . json-ts-mode)
           (python-mode . python-ts-mode)
           (rust-mode . rust-ts-mode)
           (typescript-mode . typescript-ts-mode)
           (yaml-mode . yaml-ts-mode)))))

(add-to-list 'auto-mode-alist '("/\\.envrc.*" . bash-ts-mode))

;; TREEMACS
(use-package treemacs
  :straight t
  :after doom-themes
  :config
  (doom-themes-treemacs-config)
  (setq treemacs-file-extension-regex treemacs-first-period-regex-value
        treemacs-missing-project-action 'remove
        treemacs-read-string-input 'from-minibuffer
        treemacs-show-cursor t
        treemacs-width 35)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t) (treemacs-git-mode 'deferred))
    (`(t . _) (treemacs-git-mode 'simple)))
  (treemacs-hide-gitignored-files-mode nil)
  :general
  (:states 'normal
   :prefix "\\"
   "tt" 'treemacs
   "ts" 'treemacs-select-window)
  (:keymaps 'treemacs-mode-map
   [mouse-1] #'treemacs-single-click-expand-action))

(use-package treemacs-evil
  :after treemacs evil
  :straight t)

(use-package treemacs-projectile
  :after treemacs projectile
  :straight t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :straight t)

(use-package treemacs-magit
  :after treemacs magit
  :straight t)

;; NEOTREE
;; (use-package neotree
;;   :straight t
;;   :commands (neotree-toggle neotree-projectile-action)
;;   :config
;;   (setq neo-banner-message nil
;;         neo-smart-open t
;;         neo-create-file-auto-open t
;;         neo-window-fixed-size nil
;;         neo-window-width 30)
;;   :general
;;   (:states 'normal
;;    :prefix "\\"
;;    "tt" 'neotree-toggle
;;    "ts" 'neotree-show
;;    "tp" 'neotree-projectile-action))

;; DELIMITERS
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; UNDO TREE
(use-package undo-tree
  :straight t
  :diminish
  :hook (evil-local-mode . turn-on-undo-tree-mode)
  :init
  (global-undo-tree-mode)
  :config
  (let ((undo-dir (concat user-emacs-data-directory "/undo")))
    (unless (file-directory-p undo-dir)
      (mkdir undo-dir t))
    (setq undo-tree-auto-save-history t
          undo-tree-history-directory-alist `(("." . ,undo-dir))))
  :general
  (:prefix "\\" :states 'normal "u" #'undo-tree-visualize))

;; BETTER BUFFER NAMES
(use-package uniquify
  :straight (:type built-in)
  :config
  (setq uniquify-buffer-name-style 'forward))

;; SPELL CHECKING
(use-package jinx
  :straight t
  :hook (emacs-startup . global-jinx-mode)
  :general
  (:states 'normal
   "z=" #'jinx-correct
   "]s" #'jinx-next
   "[s" #'jinx-previous))

;; GIT
(use-package magit
  :straight t
  :hook (magit-process-mode . goto-address-mode)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-format-file-function #'magit-format-file-nerd-icons
        magit-popup-show-common-commands nil)
  :general
  (:prefix "\\" :states 'normal
   "g" #'magit-dispatch
   "G" #'magit-status))

(use-package magit-todos
  :straight t
  :after magit
  :config (magit-todos-mode 1))

(use-package forge
  :straight t
  :after magit
  :init
  (evil-collection-forge-setup))

(use-package git-gutter
  :straight t
  :hook (prog-mode . git-gutter-mode))

(use-package git-gutter-fringe
  :straight t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package git-modes :straight t)

(use-package gitignore-templates
  :straight t
  :commands (gitignore-templates-insert
             gitignore-templates-new-file))

(use-package xref
  :straight (:type built-in)
  :config
  (setq xref-prompt-for-identifier nil))

(use-package indent-bars
  :straight (:host github :repo "jdtsmith/indent-bars")
  :hook ((python-base-mode . indent-bars-mode)
         (yaml-ts-mode . indent-bars-mode))
  :config
  (setq indent-bars-color '(highlight :face-bg t :blend 0.2)
        indent-bars-color-by-depth nil
        indent-bars-display-on-blank-lines nil
        indent-bars-highlight-current-depth nil
        indent-bars-pad-frac 0.1
        indent-bars-pattern "."
        indent-bars-prefer-character t
        indent-bars-width-frac 0.1
        indent-bars-zigzag nil
        indent-bars-treesit-support t
        indent-bars-no-descend-string t
        indent-bars-treesit-ignore-blank-lines-types '("module")))

;; VERTICO
(use-package vertico
  :straight t
  :init
  (setq completion-in-region-function #'consult-completion-in-region)
  (vertico-mode)
  :config
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t
        vertico-count 20)
  (setq vertico-cycle t)
  :general
  (:keymaps 'vertico-map
   "C-<return>" 'vertico-exit))

;; MARGINALIA
(use-package marginalia
  :straight t
  :init
  (marginalia-mode)
  :config
  (setq marginalia-max-relative-age 0
        marginalia-align 'right)
  :general
  (:keymaps 'minibuffer-local-map
   "M-A" 'marginalia-cycle))

;; ORDERLESS
(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((eglot (styles . (orderless flex)))
                                        (file (styles . (partial-completion))))))

;; EMBARK
(use-package embark
  :straight t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (defun embark-consult-goto-grep (location)
    ;; OVERRIDE to use projectile
    (let ((default-directory (projectile-project-root)))
      (consult--jump (consult--grep-position location))
      (pulse-momentary-highone-light-line (point))))
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
  :straight t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (setq consult-fd-args "fd --hidden --full-path --color=never"
        consult-ripgrep-args "rg --hidden --glob \!.git --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip"
        consult-project-root-function #'projectile-project-root
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (consult-customize consult-find consult-fd :state (consult--file-preview))
  :general
  (:prefix "\\"
   :states 'normal
   "l" 'consult-buffer
   "pl" 'consult-project-buffer
   "/" 'consult-line
   "sf" 'consult-fd
   "sl" 'consult-line
   "so" 'consult-outline
   "sr" 'consult-ripgrep
   "sp" 'consult-yank-from-kill-ring))

(use-package embark-consult
  :straight t
  :after embark consult
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-eglot
  :straight t
  :after eglot consult
  :general
  (:prefix "\\"
   :states 'normal
   "ss" 'consult-eglot-symbols))

;; WGREP
(use-package wgrep :straight t)

;; CORFU
(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :diminish
  :after orderless
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.25
        corfu-auto-prefix 1
        corfu-count 14
        corfu-quit-at-boundary nil
        corfu-quit-no-match t
        corfu-cycle t
        corfu-popupinfo-delay 0.5
        corfu-popupinfo-max-height 30))

(use-package kind-icon
  :straight t
  :after corfu
  :config
  (setq kind-icon-use-icons t
        kind-icon-default-face 'corfu-default
        kind-icon-blend-background nil
        kind-icon-blend-frac 0.08)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; (use-package completion-preview
;;   :ensure nil
;;   :after corfu
;;   :hook (prog-mode . completion-preview-mode)
;;   :config
;;   (setopt completion-preview-sort-function corfu-sort-function))

;; PRESCIENT
(use-package prescient
  :straight t
  :config
  (prescient-persist-mode 1))

(use-package corfu-prescient
  :straight t
  :after corfu prescient
  :config
  (corfu-prescient-mode 1)
  (setq corfu-prescient-enable-sorting t
        corfu-prescient-override-sorting nil
        corfu-prescient-enable-filtering nil))

(use-package vertico-prescient
  :straight t
  :after vertico prescient
  :config
  (vertico-prescient-mode 1)
  (setq vertico-prescient-enable-sorting t
        vertico-prescient-override-sorting nil
        vertico-prescient-enable-filtering nil))

;; ELDOC
(use-package eldoc :ensure nil :diminish)

(use-package eldoc-box :straight t :diminish)

;; PYTHON
(use-package python-mode
  :straight t
  :mode ("\\.py\\'")
  :interpreter (("python" . python-ts-mode))
  :hook ((python-ts-mode . (lambda () (setq-local fill-column 88)))
         (before-save . eglot-format))
  :general
  (:keymaps 'python-ts-mode-map
   "TAB" #'py-indent-line))

(use-package cython-mode
  :straight t
  :mode ("\\.pyx\\'"))

;; GO
(use-package go-mode
  :straight t
  :mode ("\\.go\\'"))

;; RUST
(use-package rust-mode
  :straight t
  :hook (before-save . eglot-format)
  :mode ("\\.rs\\'"))

;; HASKELL
(use-package haskell-mode
  :straight t
  :mode ("\\.l?hs\\'"))

;; ELIXIR
(use-package elixir-ts-mode
  :straight t
  :hook ((elixir-ts-mode . eglot-ensure)
         (before-save . eglot-format))
  :mode ("\\.ex\\'"))

;; DOCKER
(use-package docker
  :straight t
  :general
  (:prefix "\\" :states 'normal "d" #'docker))

(use-package dockerfile-mode
  :straight t
  :mode ("Dockerfile"))

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
  :straight t
  :mode ("\\.php\\'"))

(use-package web-mode
  :straight t
  :mode (("\\.html\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.blade\\.php\\'" . web-mode))
  :config
  (setq web-mode-engines-alist '(("blade" . "\\.blade\\."))
        web-mode-markup-indent-offset 2))

;; HY
(use-package hy-mode
  :straight t
  :mode ("\\.hy\\'")
  :interpreter ("hy" . hy-mode))

;; SVELTE
(use-package svelte-mode
  :straight t
  :mode ("\\.svelte\\'"))

;; TYPESCRIPT
(use-package typescript-mode
  :straight t
  :mode ("\\.ts\\'"))

;; TERRAFORM
(use-package terraform-mode
  :straight t
  :mode ("\\.tf\\'")
  :hook (terraform-mode . terraform-format-on-save-mode))

;; LUA
(use-package lua-mode
  :straight t
  :mode ("\\.lua\\'")
  :interpreter ("lua" . lua-mode))

;; WEB APIS
(use-package verb :straight t)
;; (use-package restclient
;;   :straight t
;;   :mode (("\\.http\\'" . restclient-mode))
;;   :general
;;   (:keymaps 'restclient-mode-map
;;    :states 'normal
;;    :prefix "\\"
;;    "n" 'restclient-jump-next
;;    "p" 'restclient-jump-prev
;;    "s" 'restclient-http-send-current-stay-in-window
;;    "S" 'restclient-http-send-current
;;    "R" 'restclient-http-send-current-raw
;;    "y" 'restclient-copy-curl-command))

(use-package request :straight t)

(use-package graphql-mode
  :straight t
  :mode ("\\.gql\\'" "\\.graphql\\'"))

;; PROJECTILE
(use-package projectile
  :straight t
  :diminish
  :init (projectile-mode)
  :config
  (setq projectile-completion-system 'default
        projectile-switch-project-action #'consult-fd)
  :general
  (:states 'normal
   :prefix "\\"
   "pp" 'projectile-switch-project))

;; SNIPPETS
(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :straight t
  :defer t
  :after yasnippet)

;; MISE
(use-package mise
  :straight t
  :hook (after-init . global-mise-mode))

;; LANGUAGE SERVER SUPPORT
(use-package eglot
  :straight t
  :defer t
  :hook (prog-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "elixir-ls"))
  (setq read-process-output-max (* 1024 1024))
  (setq-default eglot-workspace-configuration
                '(:rust-analyzer (:procMacro (:enable t)
                                  :cargo (:allFeatures t
                                          :buildScripts (:enabled t)
                                          :loadOutDirsFromCheck t)
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
  :config (breadcrumb-mode 1))

;; LLM
(use-package gptel
  :straight t
  :config
  (setq gptel-model "gpt-4o"
        gptel-default-mode 'org-mode
        gptel-prompt-prefix-alist
        '((markdown-mode . "# ")
          (org-mode . "* ")
          (text-mode . "# ")))
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

;; WHITE SPACE
(use-package ws-butler
  :straight t
  :diminish
  :config
  (ws-butler-global-mode t)
  (setq ws-butler-keep-whitespace-before-point nil
        ws-butler-trim-predicate (lambda (_ end)
                                   (not (eq 'font-lock-string-face
                                            (get-text-property end 'face))))))

;; WINDOW MANAGEMENT
(use-package winner
  :straight (:type built-in)
  :init (winner-mode)
  :general
  (:keymaps 'evil-window-map "u" 'winner-undo))

(use-package ace-window
  :straight t
  :general
  (:keymaps 'evil-window-map "!" 'ace-window))

;; PKGBUILD
(use-package pkgbuild-mode
  :straight t
  :mode ("PKGBUILD\\'"))

;; MARKDOWN
(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)))

;; CONFIG FILES
(use-package yaml-mode :straight t :mode ("\\.yml\\'" "\\.yaml\\'"))
(use-package json-mode :straight t :mode ("\\.json\\'" "Pipfile\\.lock\\'"))
(use-package toml-mode :straight t :mode ("\\.toml\\'" "Pipfile\\'"))

;; JUST
(use-package just-mode :straight t)

(use-package justl :straight t)

;; GRPC
(use-package protobuf-mode
  :straight t
  :mode ("\\.proto\\'"))

;; SYSTEMD
(use-package systemd
  :straight t
  :mode ("\\.service\\'"
         "\\.socket\\'"
         "\\.mount\\'"
         "\\.device\\'"))

;; TERMINALS
(use-package vterm
  :straight t
  :config
  (setq vterm-buffer-name-string "*vterm %s*")
  (defun vterm-project (&optional project)
    "Open a vterm in the root of the current or specified PROJECT."
    (interactive
     (list (if current-prefix-arg
               (projectile-completing-read "Switch to project: "
                                           (projectile-relevant-known-projects))
             nil)))
    (let ((default-directory (or project (projectile-project-root))))
      (vterm)))
  (defun embark-vterm (directory)
    "Launch vterm in the specified DIRECTORY."
    (interactive "DDirectory: ")
    (let ((default-directory directory))
      (vterm)))
  :general
  (:keymaps 'embark-general-map
   "C-v" #'embark-vterm)
  (:prefix "\\"
   :states 'normal
   "v" 'vterm-project))

(use-package ansi-color
  :straight (:type built-in)
  :hook (compilation-filter . ansi-color-compilation-filter))

;; DASHBOARD
(use-package dashboard
  :straight t
  :config
  (setq dashboard-startup-banner 'logo
        dashboard-projects-backend 'projectile
        dashboard-page-separator "\n\f\n")
  (dashboard-setup-startup-hook))

(use-package page-break-lines
  :straight t
  :diminish
  :init (global-page-break-lines-mode))

;; ORG MODE
(use-package org
  :straight t
  :after doom-themes
  :mode (("\\.org\\'" . org-mode)
         ("\\.org_archive\\'" . org-mode))
  :hook
  ((org-mode . (lambda ()
                (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
                (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
                (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
                (set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch)
                (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
                (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
                (set-face-attribute 'org-meta-line nil :inherit 'fixed-pitch)
                (set-face-attribute 'org-document-info-keyword nil :inherit 'fixed-pitch)))
   (org-mode . variable-pitch-mode))
  :commands (org-capture)
  :config
  (doom-themes-org-config)
  (require 'org-tempo)
  (modify-syntax-entry ?~ "(~" org-mode-syntax-table)
  (modify-syntax-entry ?= "(=" org-mode-syntax-table)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t)
                                 (shell . t)
                                 (sql . t)))
  (setq org-tags-column 0
        org-log-done 'time
        org-deadline-warning-days 14
        org-src-fontify-natively t
        org-src-preserve-indentation nil
        org-src-window-setup 'other-window
        org-default-notes-file "~/src/org/inbox.org"
        org-confirm-babel-evaluate nil
        org-directory "~/notes"
        org-odt-preferred-output-format "docx"
        org-startup-indented t
        ;; org-agenda-files (mapcar (lambda (path) (concat org-directory path))
        ;;                          '("/inbox.org"
        ;;                            "/todo.org"
        ;;                            "/tickler.org"))
        org-refile-targets '(("~/notes/todo.org" :maxlevel . 3)
                             ("~/notes/tickler.org" :maxlevel . 2)
                             ("~/notes/inbox.org" :maxlevel . 1)
                             ("~/notes/someday.org" :level . 1))
        org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/notes/inbox.org" "Tasks") "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline "~/notes/tickler.org" "Tickler") "* %i%? \n %^t"))
        org-outline-path-complete-in-steps nil
        org-refile-use-outline-path t
        org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
        org-todo-keyword-faces
        '(("TODO")
          ("INPROGRESS" :foreground "#0089dd")
          ("WAITING" :foreground "#9f7efe")
          ("DONE")
          ("CANCELLED"))
        org-agenda-custom-commands '(("@" "Contexts"
                                      ((tags-todo "@email" ((org-agenda-overriding-header "Emails")))
                                       (tags-todo "@phone" ((org-agenda-overriding-header "Phone")))))))
  (setq org-priority-faces '((?A :foreground "#e45649")
                             (?B :foreground "#da8548")
                             (?C :foreground "#0098dd")))
  :general
  ("C-c c" 'counsel-org-capture)
  (:keymaps 'org-mode-map
   :states 'normal
   :prefix "\\"
   "nb" 'org-narrow-to-block
   "ns" 'org-narrow-to-subtree
   "ne" 'org-narrow-to-element
   "nw" 'widen))

;; (use-package denote
;;   :ensure t
;;   :hook (dired-mode . denote-dired-mode)
;;   :bind
;;   (("C-c n n" . denote)
;;    ("C-c n r" . denote-rename-file)
;;    ("C-c n l" . denote-link)
;;    ("C-c n b" . denote-backlinks)
;;    ("C-c n d" . denote-sort-dired))
;;   :config
;;   (setq denote-directory (expand-file-name "~/Documents/notes/"))

;;   ;; Automatically rename Denote buffers when opening them so that
;;   ;; instead of their long file name they have, for example, a literal
;;   ;; "[D]" followed by the file's title.  Read the doc string of
;;   ;; `denote-rename-buffer-format' for how to modify this.
;;   (denote-rename-buffer-mode 1))


(use-package evil-org
  :straight t
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

(use-package ox-rst :straight t :after ox)
(use-package ox-gfm :straight t :after ox)
(use-package ox-typst :straight (ox-typst :host github :repo "jmpunkt/ox-typst") :after ox)

(use-package org-superstar
  :straight t
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("⁖")))

(use-package org-fancy-priorities
  :straight t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '((?A . "❗")
                                    (?B . "↑")
                                    (?C . "↓"))))

(show-paren-mode 1)

(setq ring-bell-function 'ignore)

(pcase system-type
  ('darwin
   (setq-default browse-url-browser-function 'browse-url-firefox
                 browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox"))
  ('gnu/linux
   (setq-default browse-url-browser-function 'browse-url-xdg-open)))

(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

(setq user-full-name "Simon Gomizelj"
      user-mail-address "simon@vodik.xyz")

(setq compilation-scroll-output 'first-error
      comint-move-point-for-output t
      comint-input-ring-size 5000
      eshell-scroll-to-bottom-on-output t)

;; store all backup and autosave files in the tmp dir
(let ((backup-dir (concat user-emacs-data-directory "/backup")))
  (unless (file-directory-p backup-dir)
    (mkdir backup-dir t))
  (setq-default backup-directory-alist `(("." . ,backup-dir))))

;; disable lockfiles
(setq create-lockfiles nil)

(setq use-default-font-for-symbols nil)

;; don't set it globally
(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)


(global-auto-revert-mode t)

(eval-after-load "auto-revert-mode"
  '(diminish 'auto-revert-mode))

(defun sort-lines-nocase ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(setenv "DOCKER_BUILDKIT" "1")
(setenv "COMPOSE_DOCKER_CLI_BUILD" "1")

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554"
     "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "c5878086e65614424a84ad5c758b07e9edcf4c513e08a1c5b1533f313d1b17f1"
     "7758a8b8912ef92e8950a4df461a4d510484b11df0d7195a8a3d003965127abc"
     "6e18353d35efc18952c57d3c7ef966cad563dc65a2bba0660b951d990e23fc07"
     "113a135eb7a2ace6d9801469324f9f7624f8c696b72e3709feb7368b06ddaccc"
     "3c08da65265d80a7c8fc99fe51df3697d0fa6786a58a477a1b22887b4f116f62"
     "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1"
     "2b20b4633721cc23869499012a69894293d49e147feeb833663fdc968f240873"
     "a6920ee8b55c441ada9a19a44e9048be3bfb1338d06fc41bce3819ac22e4b5a1"
     "c8b3d9364302b16318e0f231981e94cbe4806cb5cde5732c3e5c3e05e1472434"
     "38c0c668d8ac3841cb9608522ca116067177c92feeabc6f002a27249976d7434"
     "c1d5759fcb18b20fd95357dcd63ff90780283b14023422765d531330a3d3cec2"
     "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0"
     "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d"
     "1f292969fc19ba45fbc6542ed54e58ab5ad3dbe41b70d8cb2d1f85c22d07e518"
     "6a5584ee8de384f2d8b1a1c30ed5b8af1d00adcbdcd70ba1967898c265878acf"
     "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36"
     "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66"
     "32f22d075269daabc5e661299ca9a08716aa8cda7e85310b9625c434041916af"
     "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9"
     "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9"
     "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1"
     "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a"
     "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d"
     "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f"
     "d6b934330450d9de1112cbb7617eaf929244d192c4ffb1b9e6b63ad574784aad"
     "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22"
     "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350"
     "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd"
     "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882"
     "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb"
     "e8ceeba381ba723b59a9abc4961f41583112fc7dc0e886d9fc36fa1dc37b4079"
     "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290"
     "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7"
     "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710"
     "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
     "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851"
     "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87"
     "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "30d174000ea9cbddecd6cc695943afb7dba66b302a14f9db5dd65074e70cc744"
     "6e33d3dd48bc8ed38fd501e84067d3c74dfabbfc6d345a92e24f39473096da3f"
     "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0"
     "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d"
     "9d5124bef86c2348d7d4774ca384ae7b6027ff7f6eb3c401378e298ce605f83a"
     "d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4"
     "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874"
     "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738"
     "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725"
     "7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d"
     "456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641"
     "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0"
     "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
     "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
     "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100"
     "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68"
     "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006"
     "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33"
     "e87fd8e24e82eb94d63b1a9c79abc8161d25de9f2f13b64014d3bf4b8db05e9a"
     "3de5c795291a145452aeb961b1151e63ef1cb9565e3cdbd10521582b5fd02e9a"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a"
     "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738"
     "c517e98fa036a0c21af481aadd2bdd6f44495be3d4ac2ce9d69201fcb2578533"
     "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039"
     "162201cf5b5899938cfaec99c8cb35a2f1bf0775fc9ccbf5e63130a1ea217213"
     "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad"
     "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8"
     "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
