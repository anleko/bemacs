;;; cfg-magit.el --- git integration with magit
;;; Commentary:
;;; Code:

(use-package magit
  :demand
  :custom-face
  (magit-hash ((t (:foreground "spring green"))))
  :config
  (setenv "GIT_PAGER" "")
  (setf magit-blame-echo-style 'lines)
  (setf magit-repository-directories '(("~/src" . 1) ("~/devel" . 3)))
  (setf magit-commit-arguments (quote ("--signoff")))
  (setf magit-set-upstream-on-push t)
  (setf magit-revert-buffers 1)
  (setf magit-log-show-refname-after-summary t)
  (setf magit-log-arguments '("--graph" "--decorate" "-n128"))
  (setf magit-log-section-arguments '("--decorate" "-n256"))
  (setf magit-completing-read-function 'ivy-completing-read)
  (setf magit-use-sticky-arguments nil)
  (setf magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  (setf magit-status-margin '(nil age magit-log-margin-width nil 18))

  (setf magit-status-headers-hook
	'(magit-insert-error-header
	  magit-insert-repo-header
	  magit-insert-remote-header
	  magit-insert-diff-filter-header
	  magit-insert-head-branch-header
	  magit-insert-upstream-branch-header
	  magit-insert-push-branch-header
	  magit-insert-tags-header))

  (setf magit-status-sections-hook
	'(magit-insert-status-headers
	  magit-insert-merge-log
	  magit-insert-rebase-sequence
	  magit-insert-am-sequence
	  magit-insert-sequencer-sequence
	  magit-insert-bisect-output
	  magit-insert-bisect-rest
	  magit-insert-bisect-log
	  magit-insert-untracked-files
	  magit-insert-unstaged-changes
	  magit-insert-staged-changes
	  magit-insert-stashes
	  magit-insert-unpulled-from-upstream
	  magit-insert-unpulled-from-pushremote
	  magit-insert-unpushed-to-upstream
	  magit-insert-unpushed-to-pushremote
	  magit-insert-modules-unpulled-from-upstream
	  magit-insert-modules-unpulled-from-pushremote
	  magit-insert-modules-unpushed-to-upstream
	  magit-insert-modules-unpushed-to-pushremote
	  magit-insert-recent-commits))
  (setf magit-save-repository-buffers 'dontask)
  (defun my-git-commit-hook-fn ()
    (setq-local git-commit-summary-max-length 50)
    (setq-local fill-column 72))
  :hook
  ((git-commit-setup . git-commit-turn-on-flyspell)
   (git-commit-mode . my-git-commit-hook-fn))
  )

(use-package magit-imerge)

(use-package git-messenger
  :config
  (setf git-messenger:show-detail t))

(use-package git-timemachine
  :straight (:host github
	     :repo "emacsmirror/git-timemachine"
	     :branch "master")
  )

(use-package git-gutter
  :config
  :demand
  (global-git-gutter-mode +1)
  )

;; helper for git-gutter in helm mode
(defun akz/--my-reshape-git-gutter (gutter)
  "Re-shape gutter for `helm'."
  (let* ((linenum-start (aref gutter 3))
         (linenum-end (aref gutter 4))
         (target-line "")
         (target-linenum 1)
         (tmp-line "")
         (max-line-length 0))
    (save-excursion
      (while (<= linenum-start linenum-end)
        (goto-line linenum-start)
        (setq tmp-line (replace-regexp-in-string "^[ \t]*" ""
                                                 (buffer-substring (line-beginning-position)
                                                                   (line-end-position))))
        (when (> (length tmp-line) max-line-length)
          (setq target-linenum linenum-start)
          (setq target-line tmp-line)
          (setq max-line-length (length tmp-line)))

        (setq linenum-start (1+ linenum-start))))
    ;; build (key . linenum-start)
    (cons (format "%s %d: %s"
                  (if (eq 'deleted (aref gutter 1)) "-" "+")
                  target-linenum target-line)
          target-linenum)))
;;;; FIXME BIND
(defun my-goto-git-gutter ()
  (interactive)
  (if git-gutter:diffinfos
      (helm :sources (list
		      (helm-build-sync-source "git gutters"
			:candidates (mapcar 'akz/--my-reshape-git-gutter git-gutter:diffinfos)
			:follow 1
			:resume 'noresume
			:action (lambda (lineno)
				  (goto-line lineno))))))
  )

(provide 'cfg-magit)
;;; cfg-magit.el ends here
