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

;; set org mode capture fns [maybe move to an sv-org.el?
(setq org-agenda-files (list "/home/shaunvig/org/"))
(global-set-key (kbd "C-c c") 'org-capture)

;; allow for y and n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; make scratch buffer the first buffer
(setq inhibit-startup-screen t)

;; custom set variables below

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-jump-mode ace-jump ace-window expand-region multiple-cursors undo-tree dumb-jump key-chord magit helm-projectile helm-adaptive helm helm-config powerline nyan-mode spacemacs-theme use-package))
 '(spacemacs-theme-custom-colors '((meta . "#2aa198"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
