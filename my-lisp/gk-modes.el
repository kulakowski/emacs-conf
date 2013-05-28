(defmacro with-require (symbol &rest body)
  `(when (require ,symbol nil t)
     ,@body))
(put 'with-require 'lisp-indent-function 1)


(defmacro with-load (file &rest body)
  `(when (load ,file t)
     ,@body))
(put 'with-load 'lisp-indent-function 1)


;; Adga mode.
(with-load (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate"))
  (add-hook 'agda2-mode-hook
            (lambda () (agda2-highlight-set-faces
                        'agda2-highlight-face-groups 'conor))))


;; Auctex mode.
(with-load "auctex"
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (add-hook 'TeX-mode-hook 'tex-pdf-mode)
  (add-hook 'TeX-mode-hook 'visual-line-mode)
  (setq TeX-view-program-list '(("Preview" "open -a Preview %o")))
  (setq TeX-view-program-selection '((output-pdf "Preview"))))


;; D mode.
(with-load "d-mode"
  (add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode)))


;; Ediff mode.
(with-require 'ediff
  (setq ediff-split-window-function 'split-window-horizontally))

;; Haskell mode.
(with-load "haskell-site-file"
  (setq haskell-program-name
        "~/code/repos/ghc/inplace/bin/ghc-stage2 --interactive -XTypeHoles")
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))


;; IRC mode.
(with-require 'rcirc
  (setq rcirc-default-nick "kulakowski")
  (setq rcirc-default-user-name "George Kulakowski")
  (setq rcirc-default-full-name rcirc-default-user-name)
  (setq rcirc-authinfo '(("freenode" nickserv "kulakowski" "openopen")))
  (setq rcirc-server-alist '(("irc.freenode.net"
                              :channels
                              ("#haskell" "#coq" "#agda"))))
  (add-hook 'rcirc-mode-hook 'rcirc-omit-mode)
  (defun-rcirc-command reopen ()
    "Restart the IRC connection."
    (interactive "i")
    (let* ((process
            (or process (with-current-buffer "*irc.freenode.net*"
                          (rcirc-buffer-process))))
           (server (car (process-contact process)))
           (port (process-contact process :service))
           (nick (rcirc-nick process))
           channels)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (eq process (rcirc-buffer-process))
            (if (rcirc-channel-p rcirc-target)
                (setq channels (cons rcirc-target channels))))))
      (delete-process process)
      (rcirc-connect server port nick
                     rcirc-default-user-name rcirc-default-full-name
                     channels)))
  (defun-rcirc-command close (reason)
    "Close and clean up the IRC connection."
    (interactive "i")
    (let ((process
           (or process (get-process "irc.freenode.net"))))
      (rcirc-cmd-quit reason process)
      (rcirc-track-minor-mode 0)))
  (defun rcirc-open ()
    (if (get-buffer "*irc.freenode.net*")
        (rcirc-cmd-reopen)
      (rcirc nil))
    (setq rcirc-activity nil)
    (rcirc-update-activity-string)
    (rcirc-track-minor-mode 1))
  (defun rcirc-toggle (&optional arg)
    (interactive "P")
    (if arg
        (rcirc-cmd-close "")
      (rcirc-open))))


;; Miranda mode.
(with-require 'miranda-mode)


;; Ocaml mode.
(with-load "tuareg"
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
  (add-hook 'tuareg-mode-hook (lambda () (setq indent-tabs-mode nil))))


;; Proof General mode.
(with-load "proof-site"
  (setq proof-shell-process-connection-type nil)
  (setq proof-splash-enable nil)
  (setq proof-three-window-enable t))


;; Racket mode.
(with-require 'quack)


;; Scala mode.
(with-load "scala-mode-auto")


;; SML mode.
(with-load "sml-mode")


;; Twelf mode.
(let ((twelf-root "~/code/repos/twelf/"))
  (with-load (concat twelf-root "emacs/twelf-init.el")))


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


;; Mode line.
(with-require 'diminish
  (diminish 'global-whitespace-mode)
  (diminish 'abbrev-mode "Abr")
  (diminish 'rcirc-omit-mode " O"))


(provide 'gk-modes)
