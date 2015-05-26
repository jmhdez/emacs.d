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

;; Real time syntax checking
(require 'flycheck)

;; Remove default jshint flychecker since we are going to use eslint
(setq-default flycheck-disabled-checkers
			  (append flycheck-disabled-checkers '(javascript-jshint)))

(setq-default flycheck-disabled-checkers
			  (append flycheck-disabled-checkers '(json-jsonlist)))

;; Add hook to enable eslint
;; WARN: libxml2 is required in windows:
;; http://sourceforge.net/projects/ezwinports/files/
(add-hook 'js2-mode-hook
          (lambda ()
			(flycheck-select-checker 'javascript-eslint)
			(flycheck-mode t)))


;; Autocomplete braces
(add-hook 'js2-mode-hook 'electric-pair-mode)
