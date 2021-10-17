;; Note:

;; Tool selection may be jedi, or anaconda-mode. This script settle it
;; down with anaconda-mode.
(use-package company
 :ensure t
 :config
 (setq company-idle-delay 0
       company-minimum-prefix-length 2
       company-show-numbers t
       company-tooltip-limit 10
       company-tooltip-align-annotations t
       ;; invert the navigation direction if the the completion popup-isearch-match
       ;; is displayed on top (happens near the bottom of windows)
       company-tooltip-flip-when-above t)
 (global-company-mode t)
 )

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  )

(use-package company-anaconda
  :ensure t
  :init (require 'rx)
  :after (company)
  :config
  (add-to-list 'company-backends 'company-anaconda)
  )
(use-package company-quickhelp
  ;; Quickhelp may incorrectly place tooltip towards end of buffer
  ;; See: https://github.com/expez/company-quickhelp/issues/72
  :ensure t
  :config
  (company-quickhelp-mode)
  )

(use-package ein
  :ensure t
  :config
  ;; (add-hook 'ein:notebook-mode (lambda () (message "hello")))
  ;; (setq ein:polymode t)
  )



(provide 'sv-python)
