;;; cfg-dired.el --- directory browser
;;; Commentary:
;;; Code:

(use-package dired
  :straight nil
  :requires ( dired-sort)
  :bind (("C-d" . dired)
	 :map dired-mode-map
	 ("RET" . ired-find-alternate-file)
	 ("K" . dired-k)
	 ("g" . dired-k))
  :init (setq-default diredp-hide-details-initially-flag nil
		      dired-dwim-target t
		      ;;omit boring auto save files in dired views
		      dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$")
  :config ;; after loading dired, do this stuff
  (setf dired-recursive-deletes 'always
	dired-recursive-copies 'always
	dired-listing-switches "-alh")
)

(defun mmm/dired-up-dir ()
  "Jump to parent directory in dired."
  (interactive)
  (find-alternate-file ".."))

(use-package diredfl
  :config
  (add-hook 'dired-mode-hook 'diredfl-mode)
  :custom-face
  (diredfl-file-name ((t (:foreground "SpringGreen"))))
  (diredfl-dir-name ((t (:foreground "DeepSkyBlue"))))
  (diredfl-dir-heading ((t (:foreground "LightBlue"))))
  (diredfl-ignored-file-name ((t (:foreground "#7F9F7F"))))
  )

(use-package dired-collapse)
(use-package dired-rainbow)

(use-package dired-hacks-utils)

(provide 'cfg-dired)
;;; cfg-dired.el ends here
