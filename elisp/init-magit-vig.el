;;; init-magit-vig.el --- My startup file for magit. -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:
;;; magit config

(use-package magit

  :init
  (progn
    (with-eval-after-load 'info
      (info-initialize)
      (add-to-list 'Info-directory-list
		   "~/.emacs.d/site-lisp/magit/Documentation/"))

					; perfect magit display config
    (setq magit-display-buffer-function
	  (lambda (buffer)
	    (display-buffer
	     buffer
	     (cond ((and (derived-mode-p 'magit-mode)
			 (eq (with-current-buffer buffer major-mode)
			     'magit-status-mode))
		    nil)
		   ((memq (with-current-buffer buffer major-mode)
			  '(magit-process-mode
			    magit-revision-mode
			    magit-diff-mode
			    magit-stash-mode))
		    nil)
		   (t
		    '(display-buffer-same-window))))))


    (defun split-window-prefer-vertically (window)
      "If there's only one window (excluding any possibly active minibuffer), then split [current] WINDOW vertically."
      (if (and (one-window-p t)
	       (not (active-minibuffer-window)))
	  (let ((split-width-threshold nil))
	    (split-window-sensibly window))
	(split-window-sensibly window)))
    )

    (setq split-height-threshold 40)
    (setq split-width-threshold 100)
    (global-set-key (kbd "C-x g") 'magit-status)
    (setq split-window-preferred-function 'split-window-prefer-vertically)

    (add-to-list 'display-buffer-alist
                 '(".*COMMIT_EDITMSG". ((display-buffer-pop-up-window) .
                                        ((inhibit-same-window . t))))))

(provide 'init-magit-vig)
;;; init-magit-vig.el ends here
