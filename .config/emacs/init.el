;;============\
;; Anukranan  |
;; init.el    |
;;============/

(setq user-init-file "~/.config/emacs/init.el"
      user-emacs-directory "~/.config/emacs")

;;============================================
;; Functions.
;;============================================

;;-------------
;; Mode-specific.
;;-------------

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

;;-------------
;; Settings.
;;-------------

(defun set-tabs-i (width)
  "Set tab width."
  (interactive "nTab width: ")
  (setq-default tab-width width
                tab-stop-width width
                tab-stop-list (number-sequence width 120 width)
                c-basic-indent width
                c-basic-offset width))

;; TODO: Make this work, and possibly make it apply automatically when
;; it detects a large amount of tabs in a file.
(defun set-reasonable-style-i ()
  "Set buffer to sane values (use 8 width tabs, line arguments up by tabs)."
  (interactive)
  (lambda ()
    (progn
      (setq indent-tabs-mode t
            c-default-style "linux")
      (set-tabs-i 8)
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

(defun add-hook-i (hook-list fn)
  "Apply a function to one or more hooks."
  (mapc (lambda (hook)
          (add-hook hook fn))
        hook-list))


;;============================================
;; Package management.
;;============================================

;;-------------
;; Archives.
;;-------------

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("gnu"    . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa"  . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(require 'use-package)
(setq use-package-always-ensure t)

;;-------------
;; Package list.
;;-------------

;; Development environment.
(use-package lsp-mode)
(use-package lsp-ui)
(use-package lsp-ivy)
(use-package company)
(use-package company-box)
(use-package projectile)
(use-package counsel-projectile)
(use-package magit)

(use-package lsp-java)
(use-package lsp-haskell)
(use-package lsp-latex)
(use-package auctex)
(use-package slime)
(use-package ess)

(use-package ewal)
(use-package ewal-spacemacs-themes)


;;============================================
;; Theming.
;;============================================

(setq inhibit-startup-message t
      visible-bell t)
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(set-fringe-mode 5)

(set-frame-font "Termsyn 11" nil t)

(use-package ewal
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p t))
(use-package ewal-spacemacs-themes
  :init (progn
          (setq spacemacs-theme-underline-parens t)
          (show-paren-mode +1)
          (global-hl-line-mode))
  :config (progn
            (load-theme 'ewal-spacemacs-classic t)
            (enable-theme 'ewal-spacemacs-classic)))


;;============================================
;; Basic.
;;============================================

;;-------------
;; General.
;;-------------

(setq-default indent-tabs-mode nil
              backward-delete-char-untabify-method 'hungry
              column-number-mode t
              display-fill-column-indicator-column 80
              show-trailing-whitespace nil)

(add-hook-i '(prog-mode-hook) '(lambda () (setq show-trailing-whitespace t)))
(add-hook-i '(prog-mode-hook text-mode-hook org-mode-hook)
            'display-line-numbers-mode)
(add-hook-i '(prog-mode-hook text-mode-hook org-mode-hook)
            'display-fill-column-indicator-mode)

(set-tabs-i 2)
(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")

;;-------------
;; IDE configuration.
;;-------------

;; Performance suggestions from the lsp-mode website.
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)
      lsp-idle-delay 0.500
      lsp-log-io nil)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode                 . lsp)
         (c++-mode               . lsp)
         (java-mode              . lsp)
         (lisp-mode              . lsp)
         (scheme-mode            . lsp)
         (haskell-mode           . lsp)
         (shell-script-mode      . lsp)
         (ess-r-mode             . lsp)
         (latex-mode             . lsp)
         (makefile-mode          . lsp)
         (makefile-automake-mode . lsp)))
(use-package lsp-ui
  :hook   (lsp-mode . lsp-ui-mode)
  :custom (lsp-ui-doc-position 'bottom))
(use-package lsp-treemacs
  :after lsp)

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map ("<tab>" . company-complete-selection))
        (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
  :custom (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init (setq projectile-switch-project-action #'projectile-dired)
  (when (file-directory-p "~/var/doc/src")
    (setq projectile-project-search-path '("~/var/doc/src"))))
(use-package counsel-projectile
  :config (counsel-projectile-mode))


;;============================================
;; Other.
;;============================================
;; Ascii art from https://www.gnu.org/graphics/gnu-ascii.html
;;  ,           ,
;; /             \
;;((__-^^-,-^^-__))
;; `-_---' `---_-'
;;  `--|o` 'o|--'
;;     \  `  /
;;      ): :(
;;      :o_o:
;;       "-"
;;  ,           ,
;; /             \
;;((__-^^-,-^^-__))
;; `-_---' `---_-'
;;  `--|o` 'o|--'
;;     \  `  /
;;      ): :(
;;      :o_o:
;;       "-"
;;  ,           ,
;; /             \
;;((__-^^-,-^^-__))
;; `-_---' `---_-'
;;  `--|o` 'o|--'
;;     \  `  /
;;      ): :(
;;      :o_o:
;;       "-"
;;  ,           ,
;; /             \
;;((__-^^-,-^^-__))
;; `-_---' `---_-'
;;  `--|o` 'o|--'
;;     \  `  /
;;      ): :(
;;      :o_o:
;;       "-"

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "8a379e7ac3a57e64de672dd744d4730b3bdb88ae328e8106f95cd81cbd44e0b6" "2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" "e28d05d3fdc7839815df9f4e6cebceb4a0ca4ed2371bee6d4b513beabee3feb7" "edf5e3ea8b3bbb4602feef2dfac8a6d5dae316fb78e84f360d55dfda0d37fa09" default))
 '(package-selected-packages
   '(auctex slime counsel-projectile projectile company-box ewal ess lsp-latex lsp-java lsp-haskell magit company lsp-ivy lsp-ui lsp-mode ewal-spacemacs-themes))
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
