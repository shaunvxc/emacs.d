(use-package eglot
  :hook
  (python-mode . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c ." . eglot-code-actions)
              ("C-c e r" . eglot-rename)
              ("C-c e f" . eglot-format)
              ("M-?" . xref-find-references)
              ("M-." . xref-find-definitions)
              ("C-c x a" . xref-find-apropos)
              ("C-c f n" . flymake-goto-next-error)
              ("C-c f p" . flymake-goto-prev-error)
              ("C-c f d" . flymake-show-project-diagnostics))
  :custom
  (eglot-autoshutdown t)
  (eglot-menu-string "LSP")
  ;; (setq lsp-use-plists t)
  :config
  (setq eldoc-echo-area-prefer-doc-buffer t
	eldoc-echo-area-use-multiline-p nil)
  )

(provide 'sv-eglot)
