(package-initialize)

(setq inhibit-startup-echo-area-message "gk")

(defmacro with-require (symbol &rest body)
  `(when (require ,symbol nil t)
     ,@body))
(put 'with-require 'lisp-indent-function 1)


(defmacro with-load (file &rest body)
  `(when (load ,file t)
     ,@body))
(put 'with-load 'lisp-indent-function 1)


(with-require 'ediff
  (setq ediff-split-window-function 'split-window-horizontally))

(with-require 'markdown-mode
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(with-require 'whitespace
  (global-whitespace-mode)
  (setq whitespace-tab whitespace-empty)
  (setq whitespace-style '(face empty trailing tabs)))

(with-require 'frame
  (defun gk-set-xterm-mouse-mode (&optional frame)
    "Turn xterm-mouse-mode on in the given (or default) frame.
Only used when window-system is nil."
    (unless window-system
      (when (or frame xterm-mouse-mode)
        (xterm-mouse-mode 1))))
  (gk-set-xterm-mouse-mode)
  (add-hook 'after-make-frame-functions 'gk-set-xterm-mouse-mode))

(setq inhibit-startup-screen t
      initial-scratch-message nil
      inhibit-splash-screen t)

(menu-bar-mode 0)

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z C-b") 'browse-url)
(global-set-key (kbd "C-z C-e") 'eshell)
(global-set-key (kbd "C-z C-s") 'shell)
(global-set-key (kbd "C-z C-z") 'suspend-frame)

(global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 5)))
(global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 5)))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups/"))
      disabled-command-function nil
      search-whitespace-regexp nil
      require-final-newline t)

(show-paren-mode 1)
(column-number-mode)

(setq enable-recursive-minibuffers 't)

(setq eshell-directory-name "~/.emacs.d/eshell")

(setq Buffer-menu-sort-column 4)

(global-auto-revert-mode t)

(setq-default indent-tabs-mode nil)

(xterm-mouse-mode)

(custom-set-variables
 '(package-selected-packages (quote (rust-mode))))

(custom-set-faces)
