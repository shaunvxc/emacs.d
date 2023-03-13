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

(provide 'sv-lsp)
