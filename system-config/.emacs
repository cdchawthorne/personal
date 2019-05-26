(setq
  ;; Automatically follow symlinks to version-controlled files
  vc-follow-symlinks t
  backup-directory-alist `((".*" . ,(concat (getenv "XDG_DATA_HOME") "/emacs/backups")))
  auto-save-file-name-transforms `((".*" ,(concat (getenv "XDG_DATA_HOME") "emacs/autosaves") t))
  show-paren-delay 0
)

(show-paren-mode 1)

(global-set-key (kbd "C-c z") '(lambda () (interactive) (save-buffers-kill-emacs 0)))
(global-set-key (kbd "C-c x") 'kill-emacs)
(defalias 'yes-or-no-p 'y-or-n-p)
