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

(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."
  :command ("jsxhint" (eval (list (file-name-nondirectory (flycheck-save-buffer-to-temp #'flycheck-temp-file-system)))))
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (js2-mode))

(add-hook 'js2-mode-hook
          (lambda ()
			(flycheck-select-checker 'jsxhint-checker)
			(flycheck-mode t)))

;; Autocomplete braces
(add-hook 'js2-mode-hook 'electric-pair-mode)

