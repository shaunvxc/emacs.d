;;; sv-ui.el --- My Emacs Setup
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

(require 'org)

(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t)
  (custom-set-variables '(spacemacs-theme-custom-colors '((meta . "#2aa198")))))


(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode)
  (nyan-start-animation)
  )

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  )


(use-package ace-window
  :ensure t
  :defer t
  :bind (("M-p" . ace-window)))

(column-number-mode t)
(delete-selection-mode 1)
(show-paren-mode t)
(global-linum-mode 1)
(tool-bar-mode -1)
(setq ring-bell-function 'ignore)


;; prevent too much line wrapping...
'(fill-column 1000)

(setq next-line-add-newlines nil)

;; configure electric pair mode
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\{ . ?\})))

(electric-pair-mode 1)


(provide 'sv-ui)
;;; sv-ui.el ends here
