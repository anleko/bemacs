;;; init.el --- DIGITAL TRANSFORMATION EMACS configuration entry point
;;; Commentary:
;;; Code:

(defvar status-fname "/tmp/emacs-status")

(defvar *total-time* 0.0)

(defvar url-http-method nil)
(defvar url-http-data nil)
(defvar url-http-extra-headers nil)
(defvar oauth--token-data nil)
(defvar url-callback-function nil)
(defvar url-callback-arguments nil)

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  (let ((time (gensym))
        (diff (gensym)))
    `(let ((,time (current-time)))
       ,@body
       (let ((,diff (time-since ,time)))
         (setf *total-time* (+ *total-time* (float-time ,diff)))
         (message "%s - Time spent: %.06f (total: %.06f)" ',@body (float-time ,diff) *total-time*)))))

(defun display-startup-echo-area-message ()
  (message "Emacs loaded in %.01f seconds.\nDigitization -> Digitalization -> DIGITAL TRANSFORMATION" *total-time*))

(defun my-log (msg &optional append)
  (let ((inhibit-message t))
    (write-region (format "%s\n" msg) nil status-fname append)
    (message "%s" msg)))

(defun my-load (file &optional absolute-path)
  (let ((f (if absolute-path file (expand-file-name (concat "config/" file ".el") user-emacs-directory))))
    (when (file-exists-p f)
      (my-log (format "Loading %s @ %s" file (current-time-string)) t)
      (load file nil t))))

(my-log (format "Startup @ %s" (current-time-string)))

(setf load-prefer-newer t)

(add-to-list 'load-path
             (expand-file-name "config/" user-emacs-directory))

(setf custom-file (expand-file-name "config/custom.el" user-emacs-directory))

(measure-time (my-load "package-bootstrap"))

(use-package exec-path-from-shell
  :demand
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
)

(measure-time (my-load "cfg-basics"))
(measure-time (my-load "cfg-visuals"))
(measure-time (my-load "cfg-dired"))
(measure-time (my-load "cfg-projectile"))
(measure-time (my-load "cfg-help"))
(measure-time (my-load "cfg-helm"))
(measure-time (my-load "cfg-lsp"))
(measure-time (my-load "cfg-fly"))
(measure-time (my-load "cfg-treemacs"))
(measure-time (my-load "cfg-company"))
(measure-time (my-load "cfg-magit"))
(measure-time (my-load "cfg-utils"))

(measure-time (my-load custom-file))

(measure-time (my-load "cfg-keys"))

(message "Done loading init.el. Took %.06f" *total-time*)

(my-log (format "Running @ %s" (current-time-string)) t)

(provide 'init)
;;; init.el ends here
