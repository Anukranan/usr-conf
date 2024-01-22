;;============\
;; Anukranan  |
;; init.el    |
;;============/

;; * TODO:
;;   + lsp-mode   -> eglot
;;   + company    -> corfu or xref + configuration
;;   + projectile -> project.el
;;   + ivy        -> ido
;;   + dap-mode   -> realgud/gud
;;   + EDE mode and configuration?
;;   + Compilation
;;   + More Org-mode
;;   + Integrate desktop environment.
;;     - Dired configuration: direx, ranger, other
;;     - eshell configuration
;;     - system-packages.el
;;     - View media
;;     - Query YouTube videos
;;     - Play music / spotify music
;; Basically, move everything into built-in packages and only have a few
;; external packages. Reduce the size of the configuration while still making
;; it a complete system for development, productivity, computer usage, etc.

;;============================================
;; Initialization.
;;============================================

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "* Booted in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f") gcs-done)))

(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

(setq user-init-file "~/.config/emacs/init.el"
      user-emacs-directory "~/.config/emacs"
      inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'emacs-lisp-mode
      visible-bell t)

(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(set-fringe-mode  5)

(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")


;;============================================
;; Functions.
;;============================================

;; Internally defined functions are post-pended with '-i'. Functions sourced
;; externally without change are not.

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
  (progn
    (setq indent-tabs-mode t
          tab-width width
          c-basic-indent width
          c-basic-offset width)))

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


;;============================================
;; Package management.
;;============================================

(require 'package)
(setq package-enable-at-startup nil
      package-native-compile t)
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
              column-number-mode t)

(add-hook-i '(prog-mode-hook) '(lambda () (setq show-trailing-whitespace t)))
(add-hook-i '(prog-mode-hook text-mode-hook org-mode-hook)
            'display-line-numbers-mode)
(add-hook-i '(prog-mode-hook text-mode-hook org-mode-hook)
            'display-fill-column-indicator-mode)

;;-------------
;; Theming.
;;-------------

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

;;-------------
;; Desktop environment.
;;-------------



;;-------------
;; Development environment.
;;-------------

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
   '(sway auctex slime company-box ewal ess lsp-latex lsp-java lsp-haskell magit company lsp-ivy lsp-ui lsp-mode ewal-spacemacs-themes))
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
