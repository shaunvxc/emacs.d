;;; init.el -- my emacs config
;;; Commentary:
;;; byte-compile-warnings: (not free-vars)
;;; Code:

(with-no-warnings
   (require 'cl))

(setq message-log-max 10000)

;;; Package management

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; (when (executable-find "curl") ;; uncomment if this is needed again
;;   (setq helm-google-suggest-use-curl-p t))

(setq inhibit-startup-message t)
(column-number-mode t)
(delete-selection-mode 1)
(show-paren-mode t)
(global-linum-mode 1)
(global-flycheck-mode)
(tool-bar-mode -1)

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'control) ; was alot
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'alt) ;fn is control -- was control
)

;;; smooth scroll
(setq scroll-conservatively 10000)

;; prevent mouse scrolling from sucking ass
(setq mouse-wheel-scroll-amount '(0.07))
(setq mouse-wheel-progressive-speed nil)

;; turn the friggan beep noise off!
(setq ring-bell-function 'ignore)

;; Emacs will not automatically add new lines
(setq next-line-add-newlines nil)

;; prevent too much line wrapping...
'(fill-column 1000)

;; allow narrowing
(put 'narrow-to-region 'disabled nil)

;; way to similar to exit emacs, unbind it
(global-unset-key (kbd "C-x c"))

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; font for all unicode characters
(set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)

;; helm
(use-package init-helm-vig
  :load-path "elisp"
)

;; magit
(use-package init-magit-vig
  :load-path "elisp")

;; various functions & key bindings
(use-package init-fnkeys-vig
  :load-path "elisp")

;; winner
(use-package winner
  :ensure t
  :defer t
  :init
  (winner-mode 1))

;; winner
(use-package ace-window
  :ensure t
  :defer t
  :bind (("M-p" . ace-window)))

;; undo tree
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  :config
  (progn
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t))
  :bind (("C-x C-u" . undo-tree-visualize))
  )

;;; markdown-mode
(use-package markdown-mode
  :load-path "elisp"
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.mdpp$" . markdown-mode))))

(use-package csharp-mode
  :load-path "elisp"
  :init
  (progn
    (require 'flymake)
    (add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
    ))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  (nyan-mode)
  (nyan-start-animation))

(use-package dumb-jump
  :ensure t
  :config
  (progn
    (defun dumb-jump-go-autosave ()
      "Save before calling dump-jump-go."
      (interactive)
      (save-buffer)
      (dumb-jump-go)
      (recenter-top-bottom)
      )
    )
  :bind ( ("M-." . dumb-jump-go-autosave))
)

(use-package company                    ; Graphical (auto-)completion
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t)
  :diminish company-mode)

(use-package multiple-cursors                    ; multiple cursors
  :ensure t
  :bind ( ("C->" . mc/mark-next-like-this)
	  ("C-<" . mc/mark-previous-like-this)
	  ("C-c C-<" . mc/mark-all-like-this)))

(use-package expand-region                    ; expand-region
  :ensure t
  :bind ( ("C-=" . er/expand-region)))

(load-theme 'dark-mint t)

(provide 'init)
;;; init.el ends here
