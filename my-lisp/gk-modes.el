(defmacro with-require (symbol &rest body)
  `(when (require ,symbol nil t)
     ,@body))
(put 'with-require 'lisp-indent-function 1)


(defmacro with-load (file &rest body)
  `(when (load ,file t)
     ,@body))
(put 'with-load 'lisp-indent-function 1)


;; Ediff mode.
(with-require 'ediff
  (setq ediff-split-window-function 'split-window-horizontally))

;; Markdown mode.
(with-require 'markdown-mode
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))


;; Whitespace mode.
(with-require 'whitespace
  (global-whitespace-mode)
  (setq whitespace-tab whitespace-empty)
  (setq whitespace-style '(face empty trailing tabs)))


;; Xterm mouse mode.
(with-require 'frame
  (defun gk-set-xterm-mouse-mode (&optional frame)
    "Turn xterm-mouse-mode on in the given (or default) frame.
Only used when window-system is nil."
    (unless window-system
      (when (or frame xterm-mouse-mode)
        (xterm-mouse-mode 1))))
  (gk-set-xterm-mouse-mode)
  (add-hook 'after-make-frame-functions 'gk-set-xterm-mouse-mode))


(provide 'gk-modes)
