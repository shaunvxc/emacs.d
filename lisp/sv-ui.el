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

(set-frame-width (selected-frame) 180)

(use-package nlinum
  :ensure t
  )

(use-package adaptive-wrap
             :ensure t)
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)

(use-package zoom
  :ensure t
  :config
  ;; (setq zoom-size '(0.618 . 0.618))
  (setq zoom-size '(0.618 . 0.70))
  (setq zoom-ignored-buffer-name-regexps '("^*helm" "^helm" "HELM*" "HELM Occur" "^*HELM" "^HELM"))
  ;; (setq zoom-ignored-buffer-names '("HELM Occur" "HELM Projectile"))
  (zoom-mode t)
  )


(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode)
)
;; nyan mode really slows down ein ... so disable nyan cat in python notebooks

(defun in-ein-notebook ()
  (if (cl-search "ipynb" (buffer-name)) t nil )
  )

(defun turn-nyan-off-if-on()
  (when (eq nyan-mode t)
    (nyan-mode -1)
    (nlinum-mode -1)
    )
  )

(defun turn-nyan-on-if-off()
  (when (eq nyan-mode nil)
    (nyan-mode +1)
    (nlinum-mode +1)
    )
  )

(defun turn-off-nyan-in-notebook ()
  ;; (message "running the nyan check")
  (if (eq (in-ein-notebook) t) (turn-nyan-off-if-on) (turn-nyan-on-if-off))
  )

(add-hook 'window-configuration-change-hook 'turn-off-nyan-in-notebook)

(use-package all-the-icons
  :ensure t)

(setq doom-modeline-major-mode-icon t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

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

;; from @cyrus-and's configs
(custom-set-variables
 '(frame-resize-pixelwise t)
 '(indicate-buffer-boundaries 'left)
 ;; '(indicate-empty-lines t)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(use-dialog-box nil)
 '(vc-follow-symlinks t)
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))

(toggle-truncate-lines -1)		;don't like truncated lines
(blink-cursor-mode 0)              ;; no blinking cursor
(add-hook 'text-mode-hook 'visual-line-mode)
(advice-add 'iconify-or-deiconify-frame :before-until 'display-graphic-p)

(setq theme-color-accent  "#ffffaf")
(setq theme-color-low     "#000000")
(setq theme-color-high    "#ffffaf")
(setq theme-color-level-1 "#1D1F21")
(setq theme-color-level-2 "#282A2E")
(setq theme-color-level-3 "#373B41")
(setq theme-color-level-4 "#C5C8C6")

;; common colors
(setq theme-color-red     "#A54242")
(setq theme-color-green   "#8C9440")
(setq theme-color-yellow  "#DE935F")
(setq theme-color-blue    "#5F819D")
(setq theme-color-magenta "#85678F")
(setq theme-color-cyan    "#5E8D87")
(setq theme-color-gray    "#707880")


;; window dividers
(custom-set-faces
 `(window-divider             ((t (:foreground ,theme-color-level-3))))
 `(window-divider-first-pixel ((t (:foreground ,theme-color-level-3))))
 `(window-divider-last-pixel  ((t (:foreground ,theme-color-level-3))))
 `(internal-border            ((t (:background ,theme-color-level-3)))))

;; others
(custom-set-faces
 `(completions-common-part      ((t (:foreground ,theme-color-level-1 :background ,theme-color-accent))))
 `(cursor                       ((t (:background ,"#ffffaf"))))
 `(warning                       ((t (:foreground ,"#ffffaf", :background , "#000000") )))
 `(error                       ((t (:foreground ,"#ffffaf", :background , "#000000") )))
 `(doom-modeline-buffer-modified ((t (:inherit (bold) :foreground, "#ffffaf"))))
 `(diff-refine-changed          ((t (:extend t))))
 `(fringe                       ((t (:inherit (shadow)))))
 `(isearch-fail                 ((t (:inherit (error)))))
 `(minibuffer-prompt            ((t (:inherit (bold) :foreground ,theme-color-accent))))
 `(pulse-highlight-start-face   ((t (:background ,theme-color-accent))))
 `(region                       ((t (:foreground ,theme-color-level-1 :background ,theme-color-high :extend t))))
 `(secondary-selection          ((t (:foreground ,theme-color-accent :background ,theme-color-high :extend t))))
 `(show-paren-match             ((t (:inherit (bold) :foreground ,theme-color-accent))))
 `(show-paren-mismatch          ((t (:inherit (error) :inverse-video t)))))

(use-package rainbow-mode
  :ensure t)

(provide 'sv-ui)
;;; sv-ui.el ends here
