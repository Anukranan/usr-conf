;;---------;;
;; init.el ;;
;;---------;;

;;=============================
;; Initialization.
;;=============================

;; TODO: Set shell variables to be sourced automatically.
(progn
  (setenv "XDG_CACHE_HOME" (concat (getenv "HOME") "/.cache")))

;; A bunch of top-level defaults (which is why they are in this section).
(setq-default
 user-emacs-directory (expand-file-name "emacs" (getenv "XDG_CONFIG_HOME"))
 user-init-file       (expand-file-name "init.el" user-emacs-directory)
 custom-file          (expand-file-name "custom.el" user-emacs-directory)

 backup-dir-i                   (concat (getenv "XDG_CACHE_HOME") "/emacs")
 backup-directory-alist         '((".*" . ,backup-dir-i))
 auto-save-file-name-transforms '((".*" . ,backup-dir-i))
 auto-save-list-file-prefix     (expand-file-name "autosave-"   backup-dir-i)
 ido-save-directory-list-file   (expand-file-name "ido_history" backup-dir-i)

 desktop-dirname        user-emacs-directory
 desktop-path           desktop-dirname
 desktop-base-file-name "emacs.desktop"
 desktop-base-lock-name (concat desktop-base-file-name ".lock")

 desktop-save                t
 desktop-save-mode           t
 desktop-load-locked-desktop nil
 desktop-auto-save-timeout   30

 backup-by-copying   t
 delete-old-versions t
 kept-new-versions   6
 kept-old-versions   2
 version-control     t

 default-frame-alist     '((font . "Termsyn-11"))
 initial-major-mode      'emacs-lisp-mode
 initial-scratch-message nil
 inhibit-startup-message t
 visible-bell        nil

 show-paren-mode     t
 global-hl-line-mode nil
 winner-mode         t)

(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)


;;=============================
;; Package management.
;;=============================

(require 'package)
(setq-default package-install-upgrade-built-in t
              package-native-compile t
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


;;=============================
;; Functions.
;;=============================

;;--------------
;; Settings.
;;--------------

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

;;--------------
;; Minor modes.
;;--------------

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

(define-minor-mode stabs-mode ()
  "Enables `indent-tabs-mode', `smart-tabs-mode', and `linux-style-mode' locally
in a buffer."
  :init-value nil
  :lighter " Stabs"
  :require 'smart-tabs-mode
  (if (not stabs-mode)
      (setq indent-tabs-mode nil
            smart-tabs-mode nil
            linux-style-mode nil)
    (setq indent-tabs-mode t
          smart-tabs-mode t
          linux-style-mode t)))

;;--------------
;; Utiltity.
;;--------------

(defun add-hook-list (hook-list function)
  "Apply a function to one or more hooks."
  (mapc (lambda (hook)
          (add-hook hook function))
        hook-list))


;;=============================
;; Configuration.
;;=============================

;;--------------
;; General settings.
;;--------------

(setq-default tab-width 8
              indent-tabs-mode nil
              backward-delete-char-untabify-method 'hungry

              fill-column 80
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

(add-hook-list '(java-mode-hook)
               #'(lambda ()
                   (setq fill-column 100
                         set-fill-column 100
                         display-fill-column-indicator-column 100)))

(add-hook-list '(c-mode-hook c++-mode-hook java-mode-hook rust-mode-hook)
               #'linux-style-mode)

;;--------------
;; Desktop environment.
;;--------------

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

;;--------------
;; Development environment.
;;--------------

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

;;--------------
;; Theming.
;;--------------

(set-fringe-mode  0)

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
