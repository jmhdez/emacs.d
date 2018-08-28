 ;; Javascript settings

;; major-mode
(add-to-list 'auto-mode-alist
             '("\\.js\\'" . rjsx-mode))

;; Minor modes
(add-hook 'rjsx-mode-hook 'electric-pair-mode)
(add-hook 'rjsx-mode-hook 'js2-refactor-mode)

;; Add support for mocha tests
(setq mocha-command ".\\node_modules\\.bin\\mocha.cmd")
(setq mocha-which-node "")
