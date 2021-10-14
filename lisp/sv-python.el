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

;; Another choice is Jedi, but it is designed to work with
;; AutoComplete instead of company-mode.  There's jedi-company, but
;; `jedi:setup` will always setup ac, unless you disable it
;; explicitly, which is annoying.  Also, you don't get the tool tip.

;; If you really want to try with jedi+company, use below scripts it
;; (remove the :disabled tag)
(use-package jedi
  :disabled
  :after (epc pos-tip)
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup)
  :config
  ;; For setup
  ;; http://d.hatena.ne.jp/n-channel/20131220/1387551080
  ;; and this:
  ;; http://kenbell.hatenablog.com/entry/2017/01/15/112344

  ;; Under windows, process might very long and EPC may fail.
  ;; Set it larger. What a bummer...
  ;;(if (memq system-type '(ms-dos windows-nt))
  ;;(setq epc:accept-process-timeout 1000))
  )

(use-package company-jedi
  :disabled
  :ensure t
  :config)

(provide 'sv-python)
