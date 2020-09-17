;;; cfg-utils.el -- utilities and functions
;;; Commentary:
;;; Code:

(defun akz/goto-line-show ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (let ((prev-linum (linum-mode)))
    (unwind-protect
	(progn
	  (unless prev-linum (linum-mode 1))
	  (call-interactively #'goto-line)
	  )
      (when prev-linum (linum-mode -1))
      )
    )
  )

(defadvice find-file (around akz/th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun akz/th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

(defun akz/ripgrep-projectile-or-directory ()
  "'ripgrep' in project tree if in a project, or in current directory otherwise."
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-rg)
    (call-interactively 'helm-rg)
    )
  )

(defun akz/new-empty-buffer ()
  "Opens a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setf buffer-offer-save t)))

(defun akz/close-current-buffer ()
  "Close the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or \"beginning-of-line\".

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive "^") ; Use (interactive) in Emacs 22 or older
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line)))
)

(use-package helm-tramp
  :config
  (setq helm-tramp-custom-connections '(/ssh:kandko@eden.test.westermo.com:/home/kandko/))
)

(provide 'cfg-utils)
;;; cfg-utils.el ends here
