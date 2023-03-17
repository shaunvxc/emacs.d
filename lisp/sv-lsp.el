;;; sv-lc.el --- My Emacs Setup
;;
;; Author: Shaun Viguerie <shaunvig114@gmail.com>
;;
;; Copyright (C) 2015-2023 Shaun Viguerie
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
;;; A minimal config for using lsp-mode

(use-package lsp-mode
    :commands (lsp lsp-deferred)
    :hook (go-mode . lsp-deferred))

(use-package lsp-ui
    :commands lsp-ui-mode
    :config (setq
             lsp-ui-doc-enable nil
             lsp-ui-peek-enable t
             lsp-ui-sideline-enable t
             lsp-ui-imenu-enable t
             lsp-ui-flycheck-enable t))

(use-package lsp-python-ms
    :init (setq lsp-python-ms-auto-install-server t)
    :hook (python-mode . (lambda ()
                           (require 'lsp-python-ms)
                           (lsp-deferred))))

;; ;; make sure envvar LSP_USE_PLISTS is set to true... this should help speed things up but need to figure out why its breaking.
;; (setq lsp-use-plists t)

(setq gc-cons-threshold 200000000)
(setq read-process-output-max (* 1024 2014)) ; 1mb
(setq lsp-idle-delay 0.500)

(setq lsp-lens-enable nil)		;will this prevent the cpu spikes?

(setq lsp-ui-sideline-enable t)
(setq lsp-ui-doc-enable nil)
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-ui-sideline-show-symbol nil)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-delay 0.5)
(setq lsp-prefer-flymake nil)
;; ;; (setq lsp-ui-peek-enable nil)
(setq lsp-enable-file-watchers nil)
(setq lsp-log-max nil)
(setq lsp-enable-links nil)
(setq lsp-eldoc-enable-hover nil)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-enable-snippet nil)

(add-hook 'xref-backend-functions #'lsp--xref-backend nil t)
(provide 'sv-lsp)
