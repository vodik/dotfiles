;;; early-init.el -*- lexical-binding: t; -*-

(let ((default-gc-threshold (* 256 1024 1024))  ;; 256mb
      (default-gc-percentage 0.2))
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.8)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-percentage default-gc-percentage
                    gc-cons-threshold default-gc-threshold))))

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq load-prefer-newer t)

(setq use-short-answers t)

(setq initial-buffer-choice nil
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      select-enable-clipboard t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(pcase system-type
  ('darwin
   (add-to-list 'default-frame-alist '(undecorated-round . t)))
  ('gnu/linux
   (set-frame-parameter nil 'alpha-background 97)
   (add-to-list 'default-frame-alist '(alpha-background . 97))))

(setq package-enable-at-startup nil
      ;; Display the architecture using gcc -march=native -Q --help=target | grep march
      cpu-architecture "znver4"
      native-comp-async-report-warnings-errors 'silent
      native-comp-jit-compilation t
      native-comp-compiler-options `("-O2"
                                     ,(format "-march=%s" cpu-architecture)
                                     ,(format "-mtune=%s" cpu-architecture)
                                     "-g0"
                                     "-fno-omit-frame-pointer"
                                     "-fno-finite-math-only")
      native-comp-driver-options '("-Wl,-z,pack-relative-relocs"
                                   "-Wl,-O2"
                                   "-Wl,--as-needed")
      straight-check-for-modifications '(check-on-save find-when-checking))

;;; early-init.el ends here
