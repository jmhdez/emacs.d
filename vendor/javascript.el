;; Javascript settings

;; major-mode
(add-to-list 'auto-mode-alist
             '("\\.js\\'" . js2-mode))

;; Autocomplete (not working?)
(add-hook 'js2-mode-hook 'ac-js2-mode)
;; Syntax highlight
(setq js2-highlight-level 3)

;; Javascript refactoring
(require 'js2-refactor)
(add-hook 'js2-mode-hook
          (lambda () (js2r-add-keybindings-with-prefix "C-c C-m")))

;; JSHint-style checking
(require 'flycheck)
(add-hook 'js2-mode-hook
          (lambda () (flycheck-mode t)))

;; Autocomplete braces
(add-hook 'js2-mode-hook 'electric-pair-mode)

