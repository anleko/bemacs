;;; cfg-helm.el --- minibuffer, selections and magic with helm
;;; Commentary:
;;; Code:

(use-package helm
  :demand
  :config
  (helm-mode 1)
  (setq helm-move-to-line-cycle-in-source nil) ; wrap around the list
  (setq helm-split-window-in-side-p t)       ; use current window, don't take over other
  )
(use-package helm-rg)
(setf helm-rg-default-extra-args "--no-ignore-vcs")
(use-package swiper-helm)

(provide 'cfg-helm)
;;; cfg-helm.el ends here
