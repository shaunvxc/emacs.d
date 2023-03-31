;;; sv-keys.el
;;; Code:
;;; Commentary:
;;; Load all autoloads for helm and its extensions
;;

(use-package key-chord
  :ensure t
  :config
  (key-chord-define-global "pf" 'projectile-find-file)
  (key-chord-define-global "pb" 'helm-projectile-switch-to-buffer)
  (key-chord-mode +1)
  )

;; (use-package dumb-jump
;;   :ensure t
;;   :config
;;   (setq dumb-jump-force-searcher 'ag)
;;   (setq dumb-jump-mode 1)

;;   (progn
;;     (defun dumb-jump-go-autosave ()
;;       "Save before calling dump-jump-go."
;;       (interactive)
;;       (save-buffer)
;;       ;; (dumb-jump-go)
;;       ;; (xref-find-definitions (symbol-name (symbol-at-point)))
;;       (let ((xref-prompt-for-identifier nil)) (call-interactively #'xref-find-definitions))
;;       (recenter-top-bottom)
;;       )
;;     )
;;   :bind ( ("M-." . dumb-jump-go-autosave))
;;   )

(defun sv-xref-go-back ()
  "recenter after calling xref-go-back"
  (interactive)
  (xref-go-back)
  (recenter-top-bottom))

(global-set-key (kbd "M-,") 'sv-xref-go-back)
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-audo-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :bind (("C-z" . undo-tree-undo)
	 ("M-z" . undo-tree-redo)))

(use-package multiple-cursors                    ; multiple cursors
  :ensure t
  :bind ( ("C->" . mc/mark-next-like-this)
	  ("C-<" . mc/mark-previous-like-this)
	  ("C-c C-a" . mc/mark-all-like-this)))

(use-package expand-region                    ; expand-region
  :ensure t
  :bind ( ("<C-return>" . er/expand-region)))

(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-line-or-code)
  ;; ("C-a" . mwim-beginning)
  ;; ("C-e" . mwim-end)
  ("M-a" . fk/backward-sexp)
  ("M-e" . fk/forward-sexp)
  :config
  (defun fk/forward-sexp (&optional N)
    "Call `forward-sexp', fallback `forward-char' on error."
    (interactive)
    (condition-case nil
        (forward-sexp N)
      (error (forward-char N))))

  (defun fk/backward-sexp ()
    "`fk/forward-sexp' with negative argument."
    (interactive)
    (fk/forward-sexp -1)))

;; (use-package ace-jump-mode
;;   :ensure t
;;   :bind (  ("C-j" . ace-jump-word-mode)
;; 	   ("M-n" . ace-jump-line-mode))
;; )

;; CUSTOM FUNCTIONS BELOW

;; toggle-truncate lines
(global-set-key (kbd "C-x C-l") 'toggle-truncate-lines)

;; define squeeze file functions
;; remove any whitespace at the ends of lines
(defun delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file, even the last one."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines)
      (let ((trailnewlines (abs (skip-chars-backward "\n\t"))))
        (if (> trailnewlines 0)
            (progn
              (delete-char trailnewlines)))))))

(defun squeeze-file ()
  "Deletes all stray whitespace from the current buffer."
  (interactive)
  (delete-trailing-whitespace)
  (delete-trailing-blank-lines))

(defun save-and-squeeze ()
  "Deletes all stray whitespace from the current buffer."
  (interactive)
  (squeeze-file)
  (save-buffer))

(global-set-key (kbd "C-x C-s") 'save-and-squeeze)
;;; maps the key-binding for the function that removes all white space
(global-set-key [(ctrl x) (w)] 'squeeze-file)

;; customize the behaviro of transpose chars
(defun my-transpose-chars ()
  "Scroll the other window one line down."
  (interactive)
  (backward-char)
  (transpose-chars 1)  )

;; adjust transpose-chars to switch previous two characters
(global-set-key (kbd "C-t") 'my-transpose-chars)

;; helm do git grep
(defun my-helm-grep-do-git-grep (not-all)
  (interactive "P")
  (helm-grep-git-1 default-directory (null not-all)))

(global-set-key (kbd "C-0") 'my-helm-grep-do-git-grep)


;; window navigation keys
(defun prev-window ()
  (interactive)
  (other-window -1))


(defun vig-windows ()
  "render my preferred frame configuration"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (find-file "~/.emacs.d/init.el")
  (other-window 1)
  (split-window-right)

  ;; (if (get-buffer "*ein: http://127.0.0.1:8888/scratch.ipynb*")
  ;;     (switch-to-buffer "*ein: http://127.0.0.1:8888/scratch.ipynb*")
  ;;   (switch-to-buffer "*scratch*"))

  ;; (switch-to-buffer "*scratch*")
  ;; (find-file "~/src/notebooks/")
  (other-window 1)
  ;; (find-file "~/src/sithquant/sithquant/futures/basket.py")
  (find-file "~/src/sithquant/")
  (magit-status)
  (other-window -1)
  (other-window -1)
  (split-window-below)
  (org-agenda-list)
  (other-window 1)
  (other-window 1)
  )

(defun sv-open-nb ()
  " [semi]... quick open ein files.... would be easier to inser an ein nb prefix to helm buffers"
  (interactive)
  ;; (message (concat "*ein: http://127.0.0.1:8888/" (read-string "enter the nb-name:" nil 'my-history) ".ipynb*")  )
  (let ((nb-name (concat "*ein: http://127.0.0.1:8888/" (read-string "enter the nb-name:" nil 'my-history) ".ipynb*")))

    (if (get-buffer nb-name)
	(switch-to-buffer nb-name)
      (switch-to-buffer "*scratch*"))
    ))

(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-,") 'prev-window)
(global-set-key (kbd "C-;") 'other-frame)
(global-set-key (kbd "C-x v") 'vig-windows)
(provide 'sv-keys)
;;; sv-keys.el ends here
