;;; cfg-visuals.el --- visual/graphical settings
;;; Commentary:
;;; Code:

;;; hide scroll bars
(if (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(global-hl-line-mode 1)
(make-variable-buffer-local 'global-hl-line-mode)
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)
(setq-default cursor-in-non-selected-windows nil)

(setf scroll-margin 2)
(setf scroll-step 1)
(setf scroll-conservatively 10000)
(setf scroll-error-top-bottom t)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
;;(setq scroll-preserve-screen-position 'always)

(setf show-paren-delay 0)
(show-paren-mode t)
(setf show-paren-style 'mixed)

(use-package all-the-icons
  :config (setq all-the-icons-scale-factor 0.9)
  )

(use-package all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup))

(use-package fontawesome
  :config
  (defun insert-fontawesome ()
    (interactive)
    (insert (call-interactively 'fontawesome))))

(use-package rainbow-delimiters
  :custom-face
  (rainbow-delimiters-base-face ((t (:inherit nil))))
  (rainbow-delimiters-depth-1-face ((t (:foreground "yellow"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "dark orange"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "medium spring green"))))
  (rainbow-delimiters-depth-4-face ((t (:foreground "hot pink"))))
  (rainbow-delimiters-depth-5-face ((t (:foreground "DarkOrange1"))))
  (rainbow-delimiters-depth-6-face ((t (:foreground "white smoke"))))
  (rainbow-delimiters-depth-7-face ((t (:foreground "indian red"))))
  (rainbow-delimiters-depth-8-face ((t (:foreground "navajo white"))))
  (rainbow-delimiters-depth-9-face ((t (:foreground "dodger blue"))))
  :hook ((prog-mode lisp-mode sly-mrepl) . rainbow-delimiters-mode)
  :config
  (setq rainbow-identifiers-cie-l*a*b*-saturation 80)
  (setq rainbow-identifiers-cie-l*a*b*-lightness 80)
  (setq rainbow-identifiers-cie-l*a*b*-color-count 255)
)


(use-package rainbow-identifiers
  :config
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
  (setq-default rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face)
  (setq-default rainbow-identifiers-cie-l*a*b*-lightness 60))

;(use-package rainbow-mode
;  :blackout
;  :config
;  ;;(add-hook 'prog-mode-hook 'rainbow-mode)
;  )

(use-package prism
  :straight (:host github :repo "alphapapa/prism.el"))

(use-package visible-mark)

(use-package visual-ascii-mode)

(use-package dimmer
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-hydra)
  (dimmer-configure-helm)
  (setf dimmer-fraction 0.33)
  (setf dimmer-watch-frame-focus-events nil)
  (setf dimmer-adjustment-mode :foreground)
  ;;(setf dimmer-use-colorspace :rgb)
  ;;(dimmer-mode nil)
  )

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-set-bar 'over)
  (setq centaur-tabs-set-bar 'under)
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-show-navigation-buttons nil)
  (centaur-tabs-group-buffer-groups)

  ; the line below includes magit buffers etc. in the group
  (centaur-tabs-group-by-projectile-project)

  (centaur-tabs-enable-buffer-reordering)
  ;; When the currently selected tab(A) is at the right of the last visited
  ;; tab(B), move A to the right of B. When the currently selected tab(A) is
  ;; at the left of the last visited tab(B), move A to the left of B
  (setq centaur-tabs-adjust-buffer-order t)

  :hook
  (dired-mode . centaur-tabs-local-mode)
  )


(setq linum-delay t)

(use-package linum-off)
(require 'linum-off)
(setq linum-disabled-modes-list
      '(eshell-mode wl-summary-mode compilation-mode org-mode text-mode dired-mode pdf-view-mode))

(add-to-list 'linum-disabled-modes-list 'swiper-helm-mode)
(add-to-list 'linum-disabled-modes-list 'helm-mode)
(add-to-list 'linum-disabled-modes-list 'swiper-mode)

(global-linum-mode t)

(use-package helm-swoop
  :config
  (setq helm-swoop-use-line-number-face t)
  (setq helm-swoop-use-fuzzy-match t)
)

(use-package powerline)
(require 'powerline)
(use-package moe-theme)
(powerline-moe-theme)
(moe-dark)
(moe-theme-set-color 'orange)
;;(moe-theme-random-color)
(which-function-mode)

(setq-default minimap-minimum-width 30)
(setq-default minimap-width-fraction 0.1)
(setq-default minimap-update-delay 2)
(setq-default minimap-window-location 'right)

(provide 'cfg-visuals)
;;; cfg-visuals.el ends here
