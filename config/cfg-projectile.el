;;; cfg-projectile.el --- project management with projectile
;;; Commentary:
;;; Code:

(use-package projectile
  :preface
  (defun projectile-project-find-function (dir)
    (let* ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))
  :config
  (setf projectile-git-command "fd . -0")
  (setf projectile-generic-command "fd . -0")
  (setf projectile-enable-caching t)
  (setf projectile-switch-project-action 'projectile-dired)
  (setf projectile-keymap-prefix (kbd "M-p"))
;;  (setf projectile-switch-project-action
;;        #'projectile-commander)
  (setf projectile-project-root-files-top-down-recurring
        (append '("compile_commands.json"
                  ".ccls")
                projectile-project-root-files-top-down-recurring))
  (setf projectile-completion-system 'helm)
  (setf projectile-globally-ignored-directories
        '(".workdir" ".cquery_cached_index" ".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".ccls-cache" "kernel-dev"))
  (def-projectile-commander-method ?s
    "Open a *shell* buffer for the project."
    (projectile-run-shell))
  (def-projectile-commander-method ?c
    "Run `compile' in the project."
    (projectile-compile-project nil))
  (with-eval-after-load 'project
    (add-to-list 'project-find-functions 'projectile-project-find-function))
  (projectile-mode)
  )

(use-package helm-projectile
  :config
  (helm-projectile-on)
  )

(provide 'cfg-projectile)
;;; cfg-projectile.el ends here
