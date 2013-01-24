(setq backup-directory-alist '(("." . "~/.emacs.d/backups/"))
      disabled-command-function nil
      require-final-newline t)

(show-paren-mode 1)
(column-number-mode)

(setq enable-recursive-minibuffers 't)

(setq eshell-directory-name "~/.emacs.d/eshell")

(setq Buffer-menu-sort-column 4)

(global-auto-revert-mode t)

(setq-default indent-tabs-mode nil)

(provide 'gk-tweaks)
