;;============\
;; Anukranan  |
;; init.el    |
;;============/

;; * TODO:
;; - ido configuration for better auto-complete.
;; - realgud for debugging?
;; - vterm / eshell configuration.
;; - Integrate more desktop environment.
;;   + Sway
;;   + Spotify / Music / Youtube / Etc.
;;   + More dired configuration.
;; - Check over everything to ensure consistency and cleanliness.

;;============================================
;; Initialization.
;;============================================

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "* Booted in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f") gcs-done)))

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold (* 2 1000 1000)
                   gc-cons-percentage 0.1
                   read-process-output-max (* 1024 1024)))
          t)

(setq user-init-file "~/.config/emacs/init.el"
      user-emacs-directory "~/.config/emacs"
      inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'emacs-lisp-mode
      visible-bell t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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

(defun tab-style-i (width)
  "Indent with tabs and set tab width."
  (interactive "nTab width: ")
  (setq indent-tabs-mode t
        tab-width width
        c-basic-indent width
        c-basic-offset width))

(defun linux-style-i (width)
  "Indent with tabs and set linux style for cc-mode."
  (interactive "nTab width: ")
  (progn
    (setq c-default-style "linux")
    (tab-style-i width)
    (c-add-style
     "linux-tabs-only"
     '("linux" (c-offsets-alist
                (arglist-cont-nonempty
                 c-lineup-gcc-asm-reg
                 c-lineup-arglist-tabs-only))))
    (c-set-style "linux-tabs-only")))

;;-------------
;; Utiltity.
;;-------------

(defun add-hook-i (hook-list fn)
  "Apply a function to one or more hooks."
  (mapc (lambda (hook)
          (add-hook hook fn))
        hook-list))

(defun xdg-open-i (file)
  "Open a file using xdg-open."
  (interactive "f")
  (let ((process-connection-type nil))
    (start-process
     "" nil shell-file-name
     shell-command-switch
     (format "nohup 1>/dev/null 2>/dev/null xdg-open %s"
             (shell-quote-argument (expand-file-name file))))))

(defun manual-open-i (application file)
  "Open a file using a specified application."
  (interactive "sApplication: \nf")
  (let ((process-connection-type nil))
    (start-process
     "" nil shell-file-name
     shell-command-switch
     (format "nohup 1>/dev/null 2>/dev/null %s %s"
             (shell-quote-argument application)
             (shell-quote-argument (expand-file-name file))))))


;;============================================
;; Package management.
;;============================================

(require 'package)
(setq package-native-compile t)
(add-to-list 'package-archives '("gnu"    . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa"  . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t
      use-package-verbose t
      use-package-expand-minimally t
      native-comp-async-report-warnings-errors nil)


;;============================================
;; Configuration.
;;============================================

;;-------------
;; General settings.
;;-------------

(setq-default tab-width 2
              indent-tabs-mode nil
              backward-delete-char-untabify-method 'hungry
              display-fill-column-indicator-column 80
              column-number-mode t
              history-length 1000
              use-dialog-box nil
              delete-by-moving-to-trash t
              create-lockfiles nil
              auto-save-default nil)

(setq show-paren-context-when-offscreen 'overlay)

(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(global-hl-line-mode)

(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")

(add-hook-i '(prog-mode-hook)
            '(lambda () (setq show-trailing-whitespace t)))
(add-hook-i '(prog-mode-hook text-mode-hook org-mode-hook)
            'display-line-numbers-mode)
(add-hook-i '(prog-mode-hook text-mode-hook org-mode-hook)
            'display-fill-column-indicator-mode)

(add-hook-i '(asm-mode) '(tab-style-i 8))
(add-hook-i '(c-mode-hook c++-mode-hook rust-mode-hook)
            '(linux-style-i 8))

;;-------------
;; Desktop environment.
;;-------------

(use-package dired
  :ensure nil
  :commands (dired)
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . dired-hide-details-mode))
  :init
  (setq dired-mouse-drag-files t)
  (setq dired-bind-jump nil)
  :config
  (setq dired-listing-switches "-AghoD --group-directories-first")
  (require 'dired-x)
  (setq dired-kill-when-opening-new-dired-buffer t))

(use-package proced)

(use-package transmission)

;;-------------
;; Development environment.
;;-------------

(use-package ido
  :ensure t
  :hook (((ido-mode) . ido-everywhere)
         ((fundamental-mode prog-mode text-mode org-mode eglot-mode)
          . ido-mode)))

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

;;-------------
;; Theming.
;;-------------

(set-frame-font "Termsyn 11" nil t)

(use-package ewal
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p t))
(use-package ewal-spacemacs-themes
  :init (progn
          (setq spacemacs-theme-underline-parens t))
  :config (progn
            (load-theme 'ewal-spacemacs-classic t)
            (enable-theme 'ewal-spacemacs-classic)))
