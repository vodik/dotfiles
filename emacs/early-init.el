;;; early-init.el -*- lexical-binding: t; -*-

(let ((default-gc-threshold (* 256 1000 1000))  ;; 256mb
      (default-gc-percentage 0.2))
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.8)
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-percentage default-gc-percentage
                    gc-cons-threshold default-gc-threshold))))

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq load-prefer-newer t)

(fset 'yes-or-no-p 'y-or-n-p)
(fset 'display-startup-echo-area-message 'ignore)

(setq initial-buffer-choice nil
      inhibit-startup-screen t
      inhibit-startup-message t
      select-enable-clipboard t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(xterm-mouse-mode 1)
(blink-cursor-mode 0)

(pcase system-type
  ('darwin
   (add-to-list 'default-frame-alist '(undecorated-round . t)))
  ('gnu/linux
   (set-frame-parameter nil 'alpha-background 97)
   (add-to-list 'default-frame-alist '(alpha-background . 97))))

(set-face-attribute 'default nil :family "Iosevka SS10 Extended" :height 100)
(set-face-attribute 'fixed-pitch nil :family "Iosevka SS10 Extended" :height 100)
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 100)

(setq package-native-compile t
      package-enable-at-startup nil)

(setq native-comp-jit-compilation t
      native-comp-async-report-warnings-errors 'silent)

(setq straight-check-for-modifications '(check-on-save find-when-checking))

;;; early-init.el ends here
