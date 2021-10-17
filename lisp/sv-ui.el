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

;; (use-package spacemacs-theme
;;   :defer t
;;   :init
;;   (custom-set-variables '(spacemacs-theme-custom-colors '((meta . "#2aa198"))))
;;   (load-theme 'spacemacs-dark t)
;;   )

(use-package dark-mint-theme
  :ensure t
  :init
  (load-theme 'dark-mint t)
  (set-cursor-color "#ffffaf"); make cursor the color of banana
  )

(setq set-cursor-color "#ffffaf")
(require 'frame)
(defun set-cursor-hook (frame)
  (set-cursor-color "#ffffaf"); make cursor the color of banana
 )

(add-hook 'after-make-frame-functions 'set-cursor-hook)

(use-package nlinum
  :ensure t
  )

(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup)
  )

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode)
)

(add-hook 'polymode-after-switch-buffer-hook (lambda (x y) (if ( cl-search "ipynb" (buffer-name))  (nyan-mode -1) (nyan-mode))))

;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-default-theme)
;;   )


(use-package ace-window
  :ensure t
  :defer t
  :bind (("M-p" . ace-window)))


(column-number-mode t)
(delete-selection-mode 1)
(show-paren-mode t)

;; (global-linum-mode -1)
(global-nlinum-mode)
(tool-bar-mode -1)

(setq ring-bell-function 'ignore)

;; prevent too much line wrapping...
;; '(fill-column 1000)

;; ;; configure electric pair mode
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\{ . ?\})))

(electric-pair-mode 1)

(provide 'sv-ui)
;;; sv-ui.el ends here
