;;; cfg-flycheck --- syntax checking with flycheck
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t
  :blackout flymake-mode
  :config
  (setf flycheck-cppcheck-suppressions '("variableScope"))
  (setf flycheck-display-errors-delay 0.3)
  (setf flycheck-idle-change-delay 1.0)
  (setf flycheck-indication-mode 'left-fringe)
  ;; (setf flycheck-display-errors-function nil)
  :hook
  (prog-mode . flycheck-mode)
  :ensure t
  :init
  (global-flycheck-mode t)
  )

(provide 'cfg-flycheck)
;;; cfg-fly.el ends here
