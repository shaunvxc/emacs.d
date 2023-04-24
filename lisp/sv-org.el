;;; sv-org.el --- My Emacs Setup
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

(setq org-todo-keywords  '((sequence
     "TODO(t)" ; doing later
     "CURR(c)" ; doing now
     "NEXT(n)" ; doing next
     "|"
     "DONE(d)" ; done
     )
    (sequence
     "WAIT(w)" ; waiting for some external change (event)
     "HOLD(h)" ; waiting for some internal change (of mind)
     "IDEA(i)" ; maybe someday
     "|"
     "NOTE(o@/!)" ; end state, just keep track of it
     "STOP(s@/!)" ; stopped waiting, decided not to work on it
     ))
)


(setq org-startup-with-inline-images t)
(setq org-agenda-window-setup 'current-window)

;; for storing/inserting links
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)

(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode)
  )

;; unsets redundent org-agenda-cycle (also bound to C-') so just use that if you want to use it
(define-key org-mode-map (kbd "C-,") nil)
(global-set-key (kbd "C-,") 'prev-window)

;; (use-package org-roam
;;   :ensure t
;;   :custom
;;   (org-roam-directory (file-truename "~/org-roam/"))
;;   (org-roam-complete-everywhere t)
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n g" . org-roam-graph)
;;          ("C-c n i" . org-roam-node-insert)
;;          ("C-c n c" . org-roam-capture)
;; 	 ;; :map org-mode-map
;; 	 ;; ("C-M-i" . completion-at-point)
;;          ;; Dailies
;;          ("C-c n j" . org-roam-dailies-capture-today))

;;   :config
;;   ;; If you're using a vertical completion framework, you might want a more informative completion interface
;;   (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;;   (org-roam-db-autosync-mode)

;;   ;; If using org-roam-protocol
;;   (require 'org-roam-protocol))

;; (use-package straight)
(use-package jupyter)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) ;; Other languages
   (shell . t)
   ;; Python & Jupyter
   (python . t)
   (jupyter . t)))

(org-babel-jupyter-override-src-block "python")

(add-to-list 'org-structure-template-alist
             '("s" . "src"))

(with-eval-after-load 'org
  (add-to-list 'org-structure-template-alist '("p" . "src python :session http://127.0.0.1:8888 :async yes")))

(setq org-src-block-faces '(("emacs-lisp" (:background "#000000"))
                            ("python" (:background "#000000"))))


(setq org-startup-with-inline-images t)

;; (defun sv-fix-inline-images ()
;;   (when org-inline-image-overlays
;;     (org-redisplay-inline-images)))

;; (eval-after-load 'org
;; 	       (add-hook 'org-babel-after-execute-hook 'sv-fix-inline-images))


(eval-after-load 'org
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)
(setq org-preserve-indentation nil)

(setq org-confirm-babel-evaluate nil)

(provide 'sv-org)
