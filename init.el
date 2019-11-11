(package-initialize)

(let ((dirs '("~/.emacs.d/site-lisp" "~/.emacs.d/my-lisp")))
  (dolist (dir dirs)
    (let ((default-directory dir))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))))

(setq inhibit-startup-echo-area-message "gk")

(require 'gk-modes)
(require 'gk-display)
(require 'gk-keys)
(require 'gk-tweaks)

(custom-set-variables
 '(package-selected-packages (quote (rust-mode))))

(custom-set-faces)
