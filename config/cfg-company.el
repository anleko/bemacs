;;; cfg-company.el --- code completion with company
;;; Commentary:
;;; Code:

(use-package company
  :demand
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-backends '(company-capf))
  )
(use-package company-quickhelp)
(use-package company-lsp
  :demand
  :config
  (push 'company-lsp company-backends)
  )

(provide 'cfg-company)
;;; cfg-company.el ends here
