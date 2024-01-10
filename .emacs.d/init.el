;============;
; Anukranan  ;
; init.el    ;
;============;

;============================================
; Package management
;============================================

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("gnu"    . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa"  . "https://melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;============================================
; Aesthetics
;============================================

; Minimal!
(setq inhibit-startup-message t
      visible-bell t)
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(set-fringe-mode 10)

; ewal (pywal theming).
(use-package ewal
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p t))
(use-package ewal-spacemacs-themes
  :init (progn
          (setq spacemacs-theme-underline-parens t
                norm-font (font-spec
                           :family "Termsyn"
                           :weight 'semi-bold
                           :size 12.0))
          (show-paren-mode +1)
          (global-hl-line-mode)
          (set-frame-font norm-font nil t)
          (add-to-list 'default-frame-alist
                       `(font . ,(font-xlfd-name norm-font))))
  :config (progn
            (load-theme 'ewal-spacemacs-classic t)
            (enable-theme 'ewal-spacemacs-classic)))


;============================================
; Basic
;============================================

; UTF-8 and indentation / whitespace.
(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(setq-default tab-width 8
              show-trailing-whitespace nil
              backward-delete-char-untabify-method 'hungry
              indent-tabs-mode nil)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

; Column stuff.
(setq-default column-number-mode t
              display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'org-mode-hook  #'display-fill-column-indicator-mode)

; Java mode.
(autoload 'jde-mode "jde" "JDE mode" t)
(setq auto-mode-alist
      (append '(("\\.java\\'" . jde-mode)) auto-mode-alist))


; C mode.
(setq-default c-default-style "linux"
              c-basic-indent 8)

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))
(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "linux-tabs-only")
            (setq-default indent-tabs-mode t)))

; Org Mode.
(require 'org)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))


;============================================
; Other
;============================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "8a379e7ac3a57e64de672dd744d4730b3bdb88ae328e8106f95cd81cbd44e0b6" "2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" "e28d05d3fdc7839815df9f4e6cebceb4a0ca4ed2371bee6d4b513beabee3feb7" "edf5e3ea8b3bbb4602feef2dfac8a6d5dae316fb78e84f360d55dfda0d37fa09" default))
 '(package-selected-packages
   '(auctex org-preview-html ccls lsp-latex lsp-java lsp-ui lsp-mode jdee magit nasm-mode auto-header ewal-spacemacs-themes ewal)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
