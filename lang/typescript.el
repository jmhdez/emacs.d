;; Use web-mode for both ts and tsx files
;; (I prefer its syntax highlighting instead of typescript-mode)
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))

(add-hook 'web-mode-hook 'setup-tide-on-web-mode)

(defun setup-tide-on-web-mode ()
  (when (member (file-name-extension buffer-file-name) '("ts" "tsx"))
	(tide-setup)
	(flycheck-mode +1)
	(setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))
	(eldoc-mode +1)
	(company-mode-on)))


;; Add web-mode to typescript-tslinst supported modes
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'typescript-tslint 'web-mode))


;; Format document before saving with the right options
;; More options here: https://github.com/Microsoft/TypeScript/blob/cc58e2d7eb144f0b2ff89e6a6685fb4deaa24fde/src/server/protocol.d.ts#L421-473

(setq tide-format-options
	  '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t	:placeOpenBraceOnNewLineForFunctions nil :tabSize 4 :convertTabsToSpaces nil))

(add-hook 'before-save-hook 'tide-format-before-save)



;; Keybindings for tide

(defun tide-set-keys () 
  (local-set-key (kbd "C-c C-t r") 'tide-rename-symbol)
  (local-set-key (kbd "C-c C-t s") 'tide-restart-server)
  (local-set-key (kbd "C-c C-t f") 'tide-references))

(add-hook 'typescript-mode-hook 'tide-set-keys)
(add-hook 'web-mode-hook 'tide-set-keys)

