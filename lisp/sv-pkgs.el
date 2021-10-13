;;; sv-pkgs.el ---
;;; Code:
;;; Commentary:
;;

(require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq package-check-signature nil)
(package-initialize)

(eval-when-compile
  (when (not (package-installed-p 'use-package))
    ;; (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

(provide 'sv-pkgs)
;;; sv-pkgs.el ends here
