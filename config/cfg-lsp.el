;;; cfg-lsp.el --- LSP mode for CCLS language server integration
;;; Commentary:
;;; Code:

(use-package lsp-mode
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :init (setq lsp-keymap-prefix "C-c l")
  :hook (
         (prog-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :demand
  :config
  (setq-default lsp-enable-indentation nil)
  (setq-default lsp-enable-on-type-formatting nil)
  (setq-default lsp-enable-text-document-color 1)
  (setq-default lsp-enable-links nil)
  (setq-default lsp-enable-folding nil)
  (setq-default lsp-enable-semantic-highlighting nil)
  )

(use-package lsp-ui :commands lsp-ui-mode)
;;(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :commands
  lsp-treemacs-errors-list
  lsp-treemacs-symbols-list
  :config
  (lsp-treemacs-sync-mode 1)
  )

(use-package ccls
  :demand
  :config
  (setf ccls-executable (executable-find "ccls")))

(provide 'cfg-lsp)
;;; cfg-lsp.el ends here
