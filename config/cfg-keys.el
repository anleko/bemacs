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
(unbind-key (kbd "M-<down-mouse-1>"))
(unbind-key (kbd "M-,"))

;;;; FUNCTION KEYS
(bind-key "<f1>" #'treemacs)                                ; toggle treemacs file tree display
(bind-key "<C-f2>" 'bm-toggle)
(bind-key "<f2>"   'bm-next)
(bind-key "<S-f2>" 'bm-previous)
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
(bind-key "M-s" 'helm-swoop)                               ; search for text in current buffer
;(bind-key "M-s" 'insert-symbol-at-point helm-map)           ;   press again to search for symbol under cursor
;(bind-key "M-S" 'insert-word-at-point helm-map)             ;   same again but for word
(bind-key "M-s" 'helm-swoop-from-isearch isearch-mode-map)
;(bind-key "M-s" 'insert-word-at-point helm-swoop-map)
(bind-key "C-p" 'akz/ripgrep-projectile-or-directory)       ; search recursively in project directory, or current directory
(bind-key "C-s" 'helm-rg)                                   ; search recursively, always in directory
(bind-key "M-g g" 'akz/goto-line-show)                      ; go to line in current file
(bind-key "M-," 'pop-global-mark)

(bind-key "C-(" #'sp-backward-sexp)
(bind-key "C-)" #'sp-forward-sexp)
(bind-key "C-S-[" 'sp-backward-up-sexp)
(bind-key "C-{" 'sp-backward-up-sexp)
(bind-key "C-S-]" 'sp-down-sexp)
(bind-key "C-}" 'sp-down-sexp)

;;;; MULTIPLE CURSORS
(define-key mc/keymap (kbd "<return>") nil)
(unbind-key "M-v" mc/keymap)
(unbind-key "C-v" mc/keymap)

(bind-key "M-<mouse-1>" 'mc/add-cursor-on-click)

(bind-key "C-c m d" 'mc/edit-lines)
(bind-key "C-c m b" 'mc/edit-beginnings-of-lines)
(bind-key "C-c m e" 'mc/edit-ends-of-lines)

(bind-key "C-c m 1" 'mc/insert-numbers)
(bind-key "C-c m a" 'mc/insert-letters)

(bind-key "C-c m ," 'mc/mark-previous-like-this)
(bind-key "C-c m ." 'mc/mark-next-like-this)

(bind-key "C-c m <" 'mc/unmark-previous-like-this)
(bind-key "C-c m >" 'mc/unmark-next-like-this)

(bind-key "C-c m l" 'mc/mark-all-dwim)
(bind-key "C-c m L" 'mc/mark-all-like-this)
(bind-key "C-c m x" 'mc/mark-more-like-this-extend)

;; (bind-key "C-c m n" 'mc/next-fake-cursor-after-point)
;; (bind-key "C-c m p" 'mc/prev-fake-cursor-before-point)
(bind-key "C-c m <prior>" 'mc/cycle-backward)
(bind-key "C-c m <next>" 'mc/cycle-forward)
(bind-key "C-c m p" 'mc/cycle-backward)
(bind-key "C-c m n" 'mc/cycle-forward)
;; (bind-key "C-c m <home>" 'mc/first-fake-cursor-after)
;; (bind-key "C-c m <end>" 'mc/furthest-cursor-after-point)


(bind-key "C-c m s" 'mc/sort-regions)
(bind-key "C-c m r" 'mc/reverse-regions)

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

(bind-key "C-x ." 'helm-imenu)

;;;; MISC SOURCE CONTROL
(bind-key "C-c b" 'git-messenger:popup-message)             ; show commit that changed line under cursor
(bind-key "C-c t" 'git-timemachine)                         ; jump between previous revisions of current file
(bind-key "C-c f" 'magit-log-buffer-file)

(provide 'cfg-keys)
;;; cfg-keys.el ends here
