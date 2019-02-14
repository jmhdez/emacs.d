 ;; Javascript settings

;; major-mode
(add-to-list 'auto-mode-alist
             '("\\.js\\'" . web-mode))

(add-hook 'web-mode-hook 'setup-js-on-web-mode )

;; Allow flycheck with javascript-eslint in web-mode.
;; Requires some manual steps:
;; - Install globally eslint using npm
;;   npm install -g eslint
;; - Create a default settings file in %HOME%/.eslintrc.js
;;   (Sample file is included in this folder)
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(defun setup-js-on-web-mode ()
  (when (member (file-name-extension (or buffer-file-name "")) '("js" "jsx"))
	(flycheck-mode)
	(setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))
	(setq web-mode-auto-quote-style 2) ; Use single quote
	(eldoc-mode +1)
	(company-mode-on)))

;; Add support for mocha tests
(setq mocha-command ".\\node_modules\\.bin\\mocha.cmd")
(setq mocha-which-node "")
