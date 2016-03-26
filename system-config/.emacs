(setq
  ;; Automatically follow symlinks to version-controlled files
  vc-follow-symlinks t
  backup-directory-alist `((".*" . ,(concat (getenv "HOME") "/utilities/emacs_backups")))
  auto-save-file-name-transforms `((".*" ,(concat (getenv "HOME") "/utilities/emacs_autosaves/\\1") t))
  show-paren-delay 0
)

(show-paren-mode 1)

(global-set-key (kbd "C-c z") '(lambda () (interactive) (save-buffers-kill-emacs 0)))
(global-set-key (kbd "C-c x") 'kill-emacs)
(defalias 'yes-or-no-p 'y-or-n-p)
