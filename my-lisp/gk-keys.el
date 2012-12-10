(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z C-b") 'browse-url)
(global-set-key (kbd "C-z C-e") 'eshell)
(global-set-key (kbd "C-z C-i") 'rcirc-toggle)
(global-set-key (kbd "C-z C-s") 'shell)
(eval-after-load "proof-script"
  '(progn
     (define-key proof-mode-map (kbd "C-c C-e") 'proof-goto-end-of-locked)
     (define-key proof-mode-map (kbd "C-c C-g") 'proof-goto-point)))
(global-set-key (kbd "C-z C-z") 'suspend-frame)

(provide 'gk-keys)
