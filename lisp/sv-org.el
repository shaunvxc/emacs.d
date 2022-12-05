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

(setq org-todo-keywords
      '((sequence "TODO" "|" "SCHEDULED" "|" "IN PROGRESS" "TESTING" "|" "DONE" "DEPLOYED")))

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


(provide 'sv-org)
