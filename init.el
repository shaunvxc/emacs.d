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
(require 'sv-lsp)

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

;; define a function to reset the state of variables
(defun custom/reset-var (symbl)
  "Reset SYMBL to its standard value."
  (set symbl (eval (car (get symbl 'standard-value)))))

;; to make the font smaller
;; (set-face-attribute 'default nil :height 100)
