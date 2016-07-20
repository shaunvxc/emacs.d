;;; init-fnkeys-vig.el --- random fns and keybindings. -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


(defun toggle-window-split ()
  "Toggle a split from horizontal to vertical and vice-versa."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

  ;; remove errant white space at the ends of file
(defun squeeze-file ()
  "Deletes all stray whitespace from the current buffer."
  (interactive)
  (delete-trailing-whitespace)
  (delete-trailing-blank-lines))

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



;; when jumping to a new line, automatically recenter the buffer to line being jumped to
(defun goto-line-center (arg line)
  "When running `goto-line ARG LINE`, also call `recenter-top-bottom`."
  (interactive "P\nnline: ")
  (universal-argument)
  (goto-line line)
  (recenter-top-bottom)
)

;; fns to scroll other frame
(defun scroll-other-window-up ()
  "Scroll the other window one line up."
  (interactive)
  (scroll-other-window -5)
)

;; rebind scroll-other-window to M-[ and M-]
(defun scroll-other-window-down ()
  "Scroll the other window one line down."
  (interactive)
  (scroll-other-window 5) ;; move by 5 lines at a time
)

;;; maps the key-binding for the function that removes all white space
(global-set-key [(ctrl x) (w)] 'squeeze-file)

(global-set-key (kbd "C-x f") 'find-file-in-project)

(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-l") 'toggle-truncate-lines)

;; unset the annoying minimize keybindings
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "C-z") nil)

;; map break-point macro to C-x p
(global-set-key [(ctrl x) (p)] 'insert_bpt)

;; comment or uncomment blocks
(global-set-key [(ctrl c) (c)] 'comment-or-uncomment-region)
(global-set-key (kbd"C-X SPC") 'pop-global-mark)

;;; insert break point
(fset 'insert_bpt
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("import pdb;pdb.set_trace()" 0 "%d")) arg)))

;; function to fix brace alignment in c# mode
(fset 'fix_cs_braces
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([18 123 67108896 18 41 right backspace 32 right 5 down] 0 "%d")) arg)))

;; map curly brace alignment macro to C-c f
(global-set-key [(ctrl c) (f)] 'fix_cs_braces)

;; add new lines for C-n if the point is at the end of the buffer
(setq next-line-add-newlines t)
(global-set-key "\M-`" 'other-frame)
(global-set-key (kbd "M-p") 'ace-window)

;; if vertical switch to horizontal & vice versa
(global-set-key (kbd "C-x |") 'toggle-window-split)

;; good key-bindings for scrolling the other window
(global-set-key (kbd "M-[") 'scroll-other-window-up)
(global-set-key (kbd "M-]") 'scroll-other-window-down)

;; multiple-cursors keybindings
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; use goto-line center
(global-set-key (kbd "M-g M-g") 'goto-line-center)

(global-set-key (kbd "C-x o") 'other-window)

(provide 'init-fnkeys-vig)
;;; init-fnkeys-vig.el ends here
