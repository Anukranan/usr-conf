;;---------;;
;; init.el ;;
;;---------;;

;;============================================
;; Initialization.
;;============================================

(setq-default user-emacs-directory "~/.config/emacs"
              user-init-file (expand-file-name "init.el" user-emacs-directory)
              custom-file (expand-file-name "custom.el" user-emacs-directory)
              backup-directory-alist '(("." . "~/.cache/emacs/saves"))
              auto-save-list-file-prefix "~/.cache/emacs/saves/autosaves-"
              ido-save-directory-list-file "~/.cache/emacs/saves/ido_history")

(setq-default backup-by-copying t
              initial-major-mode 'emacs-lisp-mode)

(desktop-save-mode 1)


;;============================================
;; Package management.
;;============================================

(require 'package)
(setq-default package-native-compile t
              native-comp-async-report-warnings-errors nil)
(add-to-list 'package-archives '("gnu"    . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa"  . "https://melpa.org/packages/"))
(package-initialize)

(require 'use-package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq-default use-package-always-ensure t
                use-package-expand-minimally t
                use-package-verbose t))


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

(defun customize-tab-width (width)
  "Change `tab-width' interactively."
  (interactive "nTab width: ")
  (setq tab-width width
        c-basic-indent width
        c-basic-offset width))

;;-------------
;; Minor modes.
;;-------------

(define-minor-mode stabs-mode ()
  "Enables `indent-tabs-mode' and `smart-tabs-mode' locally in a buffer."
  :init-value nil
  :lighter " Stabs"
  :require 'smart-tabs-mode
  (if (not stabs-mode)
      (setq indent-tabs-mode nil
            smart-tabs-mode nil)
    (setq indent-tabs-mode t
          smart-tabs-mode t)))

(define-minor-mode linux-style-mode ()
  "Sets the default `cc-mode' style to linux, and enables
`c-lineup-arglist-tabs-only'."
  :init-value nil
  :lighter " Linux"
  (if (not linux-style-mode)
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

(defun add-hook-list (hook-list function)
  "Apply a function to one or more hooks."
  (mapc (lambda (hook)
          (add-hook hook function))
        hook-list))


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
              auto-fill-function 'do-auto-fill
              show-trailing-whitespace t
              next-line-add-newlines t
              history-length 1000
              use-dialog-box nil
              delete-by-moving-to-trash t
              show-paren-context-when-offscreen 'overlay)

(fset 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")

(add-hook-list '(prog-mode-hook text-mode-hook org-mode-hook)
               #'display-line-numbers-mode)
(add-hook-list '(prog-mode-hook text-mode-hook org-mode-hook)
               #'display-fill-column-indicator-mode)

(add-hook-list '(prog-mode-hook)
               #'(lambda ()
                   (unless (derived-mode-p
                            'emacs-lisp-mode 'lisp-mode 'scheme-mode)
                     (stabs-mode 1))))
(add-hook-list '(c-mode-hook c++-mode-hook)
               #'linux-style-mode)
(add-hook-list '(java-mode-hook) ;; Technically Rust uses this style too...
               #'(lambda ()
                   (setq set-fill-column 100
                         display-fill-column-indicator-column 100)))

;;-------------
;; Desktop environment.
;;-------------

;; File manager.
(use-package dired
  :ensure nil
  :commands (dired)
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . dired-hide-details-mode))
  :init (setq dired-mouse-drag-files t
              dired-bind-jump nil)
  :bind (:map dired-mode-map ("-" . dired-up-directory))
  :config (setq dired-kill-when-opening-new-dired-buffer t
                dired-listing-switches "-ADHXro --group-directories-first"))

;; System process manager.
(use-package proced
  :ensure nil)

;; Torrent manager.
(use-package transmission
  :ensure t)

;; Emacs window manager.
(use-package popper
  :ensure t)

;; Window manager integration.
(use-package sway
  :ensure t)

;;-------------
;; Development environment.
;;-------------

;; Completion system.
(use-package ido-at-point
  :ensure t
  :init (ido-at-point-mode t))
(use-package ido-completing-read+
  :ensure t
  :init (ido-ubiquitous-mode t)
  :custom (setq magit-completing-read-function 'magit-ido-completing-read
                gnus-completing-read-function 'gnus-ido-completing-read
                ess-use-ido t))
(use-package ido
  :ensure nil
  :init (ido-mode t)
  :custom (setq ido-everywhere t
                ido-enable-flex-matching t
                ido-use-filename-at-point 'guess
                ido-create-new-buffer 'always))

;; LSP manager.
(use-package eglot
  :ensure nil
  :hook (((c-mode c++-mode rust-mode ess-r-mode java-mode) . eglot-ensure))
  :custom (progn
            (eglot-autoshutdown t)
            (eglot-events-buffer-size 0)
            (eglot-extend-to-xref nil)
            (eglot-ignored-server-capabilities
             '(:documentFormattingProvider
               :documentRangeFormattingProvider
               :documentOnTypeFormattingProvider))
            (eglot-stay-out-of '(yasnippet))))

;; Git manager.
(use-package magit
  :ensure t)

;; Dissassemble C, C++, and Fortran.
(use-package disaster
  :ensure t
  :commands (disaster)
  :bind (:map c-mode-map ("C-c d" . disaster)))


;;-------------
;; Theming.
;;-------------

;; This stuff should be outside of ewal so that I can use other themes
;; with these settings.
(setq-default visible-bell t
              inhibit-startup-message t
              initial-scratch-message nil
              default-frame-alist '((font . "Termsyn-11")))

(set-fringe-mode  0)

(scroll-bar-mode     -1)
(tool-bar-mode       -1)
(tooltip-mode        -1)
(menu-bar-mode       -1)
(global-hl-line-mode -1)
(show-paren-mode      1)

(use-package ewal
  :ensure t
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p t))
(use-package ewal-spacemacs-themes
  :ensure t
  :init (progn
          (load-theme 'ewal-spacemacs-classic t)
          (enable-theme 'ewal-spacemacs-classic))
  :custom (setq spacemacs-theme-underline-parens t))
