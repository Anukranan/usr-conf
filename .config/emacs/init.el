;;============\
;; Anukranan  |
;; init.el    |
;;============/

;; * TODO:
;; - realgud for debugging?
;; - Integrate more desktop environment.
;;   + Sway
;;   + Spotify / Music / Youtube / Etc.
;; - Check over everything to ensure consistency and cleanliness.

;;============================================
;; Initialization.
;;============================================

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "* Booted in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f") gcs-done)))

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000)
                  gc-cons-percentage 0.1
                  read-process-output-max (* 1024 1024))))

(setq user-init-file "~/.config/emacs/init.el"
      user-emacs-directory "~/.config/emacs"
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'emacs-lisp-mode
      visible-bell t)

;;(setq fancy-splash-image
;;      fancy-startup-text
;;      fancy-about-text)

(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(set-fringe-mode  5)


;;============================================
;; Functions.
;;============================================

;;-------------
;; Settings.
;;-------------

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defun change-tab-width-i (width)
  "Change `tab-width' interactively."
  (interactive "nTab width: ")
  (setq tab-width width
        c-basic-indent width
        c-basic-offset width))

;;-------------
;; Minor modes.
;;-------------

(define-minor-mode smart-tabs-style-i-mode ()
  "Enables `indent-tabs-mode' and `smart-tabs-mode' locally in a buffer."
  :init-value nil
  :global nil
  :lighter " Stabs"
  :version "29.2"
  :require 'smart-tabs-mode
  (if (not smart-tabs-style-i-mode)
      (setq indent-tabs-mode nil
            smart-tabs-mode nil)
    (setq indent-tabs-mode t
          smart-tabs-mode t)))

(define-minor-mode linux-style-i-mode ()
  "Sets the default `cc-mode' style to linux, and enables
`c-lineup-arglist-tabs-only'."
  :init-value nil
  :global nil
  :lighter " Linux"
  :version "29.2"
  (if (not linux-style-i-mode)
      (custom-reevaluate-setting 'c-default-style)
    (progn
      (setq c-default-style "linux")
      (c-add-style
       "linux-tabs-only"
       '("linux" (c-offsets-alist
                  (arglist-cont-nonempty
                   c-lineup-gcc-asm-reg
                   c-lineup-arglist-tabs-only))))
      (c-set-style "linux-tabs-only"))))

;;-------------
;; Utiltity.
;;-------------

(defun add-hook-i (hook-list function)
  "Apply a function to one or more hooks."
  (mapc (lambda (hook)
          (add-hook hook function))
        hook-list))


;;============================================
;; Package management.
;;============================================

(require 'package)
(setq package-native-compile t)
(add-to-list 'package-archives '("gnu"    . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa"  . "https://melpa.org/packages/"))
(package-initialize)

(require 'use-package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t
        use-package-verbose t
        native-comp-async-report-warnings-errors nil))


;;============================================
;; Configuration.
;;============================================

;;-------------
;; General settings.
;;-------------

(setq-default tab-width 8
              indent-tabs-mode nil
              backward-delete-char-untabify-method 'hungry
              set-fill-column 80
              display-fill-column-indicator-column 80
              column-number-mode t
              next-line-add-newlines t
              history-length 1000
              use-dialog-box nil
              delete-by-moving-to-trash t
              create-lockfiles nil
              auto-save-default nil
              show-paren-context-when-offscreen 'overlay)

(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(global-hl-line-mode)

(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")

;; Whitespace.
(add-hook-i '(prog-mode-hook) #'display-line-numbers-mode)
(add-hook-i '(prog-mode-hook text-mode-hook org-mode-hook)
            #'auto-fill-mode)
(add-hook-i '(prog-mode-hook text-mode-hook org-mode-hook)
            #'display-fill-column-indicator-mode)
(add-hook-i '(prog-mode-hook text-mode-hook org-mode-hook)
            #'(lambda () (setq show-trailing-whitespace t)))

;; Style.
(add-hook-i '(c-mode-hook c++-mode-hook java-mode-hook rust-mode-hook
                          asm-mode-hook nasm-mode-hook shell-script-mode-hook
                          makefile-mode-hook)
            #'smart-tabs-style-i-mode)
(add-hook-i '(c-mode-hook c++-mode-hook)
            #'linux-style-i-mode)
(add-hook-i '(java-mode-hook) ;; Technically Rust uses this style too...
            #'(lambda ()
                (progn
                  (setq set-fill-column 100
                        display-fill-column-indicator-column 100)
                  (change-tab-width-i 4))))

;;-------------
;; Desktop environment.
;;-------------

;; File manager.
(use-package dired
  :ensure nil
  :commands (dired)
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . dired-hide-details-mode))
  :init
  (setq dired-mouse-drag-files t
        dired-bind-jump nil)
  :bind (:map dired-mode-map ("-" . dired-up-directory))
  :config
  (require 'dired-x)
  (setq dired-kill-when-opening-new-dired-buffer t
        dired-listing-switches "-AghoD --group-directories-first"))

;; System process manager.
(use-package proced)

;; Torrent manager.
(use-package transmission)

;; Emacs window manager.
(use-package popper)

;; Window manager integration.
(use-package sway)

;; Package manager integration.
(use-package system-packages)

;;-------------
;; Development environment.
;;-------------

;; Completion system.
(use-package ido
  :ensure t
  :init (ido-mode t)
  :hook (((ido-mode) . ido-everywhere))
  :custom (setq ido-everywhere t
                ido-enable-flex-matching t
                ido-use-filename-at-point 'guess
                ido-create-new-buffer 'always))

;; LSP manager.
(use-package eglot
  :ensure t
  :hook (((c-mode c++-mode rust-mode ess-r-mode java-mode)
          . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref nil)
  (eglot-ignored-server-capabilities
   '(:documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider))
  (eglot-stay-out-of '(yasnippet)))

;; Git manager.
(use-package magit)

;;-------------
;; Theming.
;;-------------

(set-frame-font "Termsyn 11" nil t)

(use-package ewal
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p t))
(use-package ewal-spacemacs-themes
  :init (setq spacemacs-theme-underline-parens t)
  :config (progn
            (load-theme 'ewal-spacemacs-classic t)
            (enable-theme 'ewal-spacemacs-classic)))
