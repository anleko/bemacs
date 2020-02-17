;;; cfg-keys.el -- keybindings
;;; Commentary:
;;; Code:

;;;; unbind some not-very-useful defaults
(unbind-key (kbd "C-z"))                                    ; suspend-frame
(unbind-key (kbd "s-p"))                                    ; ns-print-buffer
(unbind-key (kbd "s-q"))                                    ; save-buffers-kill-emacs
(unbind-key (kbd "s-t"))                                    ; ns-popup-font-panel
(unbind-key (kbd "C-x C-c"))                                ; save-buffers-kill-terminal
(unbind-key (kbd "C-x ;"))                                  ; not sure what this does???
(unbind-key (kbd "C-v"))                                    ;
(unbind-key (kbd "M-v"))                                    ;
(unbind-key (kbd "<f1>"))                                   ; help
(unbind-key (kbd "C-l"))                                    ;
(unbind-key (kbd "C-x b"))                                  ; old and bad buffer selector
(unbind-key (kbd "C-1"))
(unbind-key (kbd "C-2"))
(unbind-key (kbd "C-3"))
(unbind-key (kbd "C-4"))
(unbind-key (kbd "C-5"))
(unbind-key (kbd "C-6"))
(unbind-key (kbd "C-7"))
(unbind-key (kbd "C-8"))
(unbind-key (kbd "C-9"))
(unbind-key (kbd "C-0"))

;;;; FUNCTION KEYS
(bind-key "<f1>" #'treemacs)                                ; toggle treemacs file tree display
(bind-key "<f5>" #'lsp-treemacs-symbols)                    ; toggle treemacs symbols list
(bind-key "<f6>" #'lsp-treemacs-references)                 ; show references of thing under cursor
(bind-key "<f7>" 'magit-status)                             ; show magit (git integration)

;;;; OPEN/CLOSE FILES
(bind-key "C-x <f4>" 'save-buffers-kill-emacs)              ; save and exit
(bind-key "C-S-n" 'akz/new-empty-buffer)                    ; create new empty buffer and switch to it
(bind-key "C-S-w" 'akz/close-current-buffer)                ; close current file
(bind-key "C-l" 'mmm/dired-up-dir dired-mode-map)           ; move directly to parent directory in dired

;;;; HELP
(bind-key "C-h f" #'helpful-callable)                       ; describe function, from list, in new window
(bind-key "C-h v" #'helpful-variable)                       ; describe variable, from list, in new window
(bind-key "C-h k" #'helpful-key)                            ; describe key, from press, in new window
(bind-key "C-h h" #'helpful-at-point)                       ; describe anything under cursor, in new window

;;;; TABS
(bind-key "<f12>" 'centaur-tabs-counsel-switch-group)       ; switch between groups of tabs
(bind-key "C-<f12>" 'centaur-tabs-toggle-groups)            ; show tab group tabs, or tabs

;;;; SEARCH / NAVIGATE
(bind-key "M-s" 'swiper-helm)                               ; search for text in current buffer
(bind-key "M-s" 'insert-symbol-at-point helm-map)           ;   press again to search for symbol under cursor
(bind-key "M-S" 'insert-word-at-point helm-map)             ;   same again but for word
(bind-key "C-p" 'akz/ripgrep-projectile-or-directory)       ; search recursively in project directory, or current directory
(bind-key "C-s" 'helm-rg)                                   ; search recursively, always in directory
(bind-key "M-g g" 'akz/goto-line-show)                      ; go to line in current file

(bind-key "C-(" #'sp-backward-sexp)
(bind-key "C-)" #'sp-forward-sexp)
(bind-key "C-S-[" 'sp-backward-up-sexp)
(bind-key "C-{" 'sp-backward-up-sexp)
(bind-key "C-S-]" 'sp-down-sexp)
(bind-key "C-}" 'sp-down-sexp)

(bind-key "<home>" 'smart-beginning-of-line)

;;;; HELM
(bind-key "M-x" #'helm-M-x)                                 ; run emacs command
(bind-key "C-x C-f" #'helm-find-files)                      ; open file
(bind-key "C-x C-b" #'helm-buffers-list)                    ; switch between open files (buffers)
(bind-key "C-x r b" #'helm-filtered-bookmarks)

(bind-key "<tab>" 'helm-execute-persistent-action helm-map) ; tab runs persistent alternate action
(bind-key "C-i" 'helm-execute-persistent-action helm-map)   ;   same, but in terminal
(bind-key "C-z" 'helm-select-action helm-map)               ; list other actions using C-z

(bind-key "M-p" 'projectile-command-map)

;;;; MISC SOURCE CONTROL
(bind-key "C-c b" 'git-messenger:popup-message)             ; show commit that changed line under cursor
(bind-key "C-c t" 'git-timemachine)                         ; jump between previous revisions of current file

(provide 'cfg-keys)
;;; cfg-keys.el ends here
