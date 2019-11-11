(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z C-b") 'browse-url)
(global-set-key (kbd "C-z C-e") 'eshell)
(global-set-key (kbd "C-z C-s") 'shell)
(global-set-key (kbd "C-z C-z") 'suspend-frame)

(global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 5)))
(global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 5)))

(provide 'gk-keys)
