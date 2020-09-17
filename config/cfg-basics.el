;;; basics.el -- basic setup & general packages
;;; Commentary:
;;; Code:

(setf inhibit-startup-message t)
(setf inhibit-splash-screen t)

(setf initial-scratch-message nil)
(setf initial-major-mode 'text-mode)

(setf enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)
(column-number-mode t)
(setf max-mini-window-height 0.5)
(setf ring-bell-function 'ignore)
(setf echo-keystrokes 0.01)
(setf mouse-autoselect-window t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq scroll-preserve-screen-position 'always)

(setq-default electric-indent-mode nil)
(setq-default electric-layout-mode nil)

(setq-default tab-width 8)
(setq-default c-basic-offset 8)
(setq-default indent-tabs-mode t)

(setq-default bidi-inhibit-bpa 1)

(delete-selection-mode 1)

(setq-default superword-mode 1)

(use-package general) ;; allows better "bind-key" form

(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode)
  )

(use-package no-littering
  :init
  (setf no-littering-etc-directory
        (expand-file-name "config/" user-emacs-directory))
  (setf no-littering-var-directory
        (expand-file-name "var/" user-emacs-directory)))

(use-package blackout
  :straight (:host github :repo "raxod502/blackout")
  :demand t
  :after (helm company)
  :config
  (blackout 'eldoc-mode)
  (blackout 'abbrev-mode)
  (blackout 'auto-revert-mode)
  (blackout 'global-auto-revert-mode)
  (blackout 'global-auto-revert-mode)
  (blackout 'global-auto-revert-mode-text)
  (blackout 'auto-revert-tail-mode)
  (blackout 'auto-revert-tail-mode-text)
  (blackout 'auto-revert-mode)
  (blackout 'auto-revert-mode-text)
  (blackout 'company-mode)
  (blackout 'git-gutter-mode)
  (blackout 'helm-mode)
  (blackout 'smartparens-mode)
  (blackout 'which-key-mode)
  )

(use-package recentf
  :config
  (setf recentf-auto-cleanup 'mode)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (recentf-mode 1))

(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setf which-key-use-C-h-for-paging t)
  (setf which-key-prevent-C-h-from-cycling t)
  (setf which-key-max-display-columns nil)
  (setf which-key-max-description-length 50))

(use-package sudo-edit)

(use-package swiper
  :config
  (defun insert-symbol-at-point ()
    (interactive)
    (insert (format "%s" (with-helm-window (thing-at-point 'symbol)))))

  (defun insert-word-at-point ()
    (interactive)
    (insert (format "%s" (with-helm-window (thing-at-point 'word)))))
  (setf swiper-include-line-number-in-search nil)
  (setf swiper-action-recenter t)
  )

(use-package uniquify ;; uniquify, for different buffers with the same filename
  :straight nil
  :ensure nil
  :config
  (setf uniquify-buffer-name-style 'forward))

(use-package multiple-cursors)

(use-package dumb-jump)

(use-package yaml-mode
  :config
   (add-hook 'yaml-mode-hook
    '(lambda ()
       (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
)

(use-package json-mode)

(provide 'cfg-basics)
;;; cfg-basics.el ends here
