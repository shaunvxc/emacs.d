;;; init.el --- My Emacs Setup
;;
;; Author: Shaun Viguerie <shaunvig114@gmail.com>
;;
;; Copyright (C) 2015-2021 Shaun Viguerie
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Entry point for my Emacs setup.  This module's main goal is to load
;; other files that do more specific setup work.
;;
;;; Code:


;; Default path to load lisp files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'sv-pkgs)
(require 'sv-helm)
(require 'sv-magit)
(require 'sv-python)
(require 'sv-keys)
(require 'sv-ui)
(require 'sv-org)


(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         ;; (lsp-mode . lsp-enable-which-key-integration)
	 )
  :commands lsp)

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda()
			 (require 'lsp-python-ms)
			 (lsp))))

(setq lsp-signature-auto-activate nil)
(setq lsp-signature-render-documentation nil)
(use-package lsp-ui)
(use-package flycheck)

(require 'lsp-ui-flycheck)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1))))


(setq lsp-ui-doc-show-with-cursor t)

(setq gc-cons-threshold 200000000)
(setq read-process-output-max (* 1024 2014)) ; 1mb
(setq lsp-idle-delay 0.500)

;; make sure envvar LSP_USE_PLISTS is set to true
(setq lsp-use-plists t)

(setq lsp-lens-enable nil)		;will this prevent the cpu spikes?

(setq lsp-ui-sideline-enable t)
(setq lsp-ui-doc-enable nil)
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-ui-sideline-show-symbol nil)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-delay 0.5)
(setq lsp-prefer-flymake nil)
;; (setq lsp-ui-peek-enable nil)
(setq lsp-enable-file-watchers nil)
(setq lsp-log-max nil)
(setq lsp-enable-links nil)
(setq lsp-eldoc-enable-hover nil)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-enable-snippet nil)

;; golang customizations for lsp-mode
(add-hook 'go-mode-hook #'lsp-deferred)
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; set org mode capture fns [maybe move to an sv-org.el?
(setq org-agenda-files '("~/org"))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-default-notes-file "~/org/tasks.org")
;; allow for y and n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; make scratch buffer the first buffer
(setq inhibit-startup-screen t)
(setq custom-file "~/.emacs.d/custom.el")
;; custom set variables below

(put 'narrow-to-region 'disabled nil)

;; to make the font smaller
;; (set-face-attribute 'default nil :height 100)
