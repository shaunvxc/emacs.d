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

(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-force-searcher 'ag)
  (setq dumb-jump-mode 1)

  (progn
    (defun dumb-jump-go-autosave ()
      "Save before calling dump-jump-go."
      (interactive)
      (save-buffer)
      (dumb-jump-go)
      (recenter-top-bottom)
      )
    )
  :bind ( ("M-." . dumb-jump-go-autosave))
)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :config
  (progn
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t))
  :bind (("C-x C-u" . undo-tree-visualize)
	 ("C-z" . undo-tree-visualize))
  )


(use-package multiple-cursors                    ; multiple cursors
  :ensure t
  :bind ( ("C->" . mc/mark-next-like-this)
	  ("C-<" . mc/mark-previous-like-this)
	  ("C-c C-<" . mc/mark-all-like-this)))

(use-package expand-region                    ; expand-region
  :ensure t
  :bind ( ("<C-return>" . er/expand-region)))


(use-package ace-jump-mode
  :ensure t
  :bind (  ("C-j" . ace-jump-word-mode)
	   ("M-n" . ace-jump-line-mode))
)

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

(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-,") 'prev-window)


(provide 'sv-keys)
;;; sv-keys.el ends here
