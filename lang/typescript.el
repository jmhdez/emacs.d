;; Use web-mode for both ts and tsx files
;; (I prefer its syntax highlighting instead of typescript-mode)
(require 'web-mode)
(require 'flycheck)
(require 'tide)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))

(add-hook 'web-mode-hook 'setup-tide-on-web-mode )

(defun setup-tide-on-web-mode ()
  (let ((extension (file-name-extension (or buffer-file-name ""))))
	(when (member extension '("ts" "tsx"))
	  (tide-setup)
	  (flycheck-mode +1)
	  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))
	  (setq web-mode-auto-quote-style 2) ; Use single quote
	  (eldoc-mode +1)
	  ;; Explicitly set checkers because flycheck is selecting the wrong checker (eslint)
	  ;; when extension is ts
	  (if (string-equal extension "ts")
		  (flycheck-select-checker 'typescript-tide)) 
	  (if (string-equal extension "tsx")
		  (flycheck-select-checker 'tsx-tide))
	  (company-mode-on))))


;; Ensure both tide an tslint checkers are used
;; https://github.com/ananthakumaran/tide/issues/95
(flycheck-add-next-checker 'tsx-tide '(warning . typescript-tslint) 'append)
(flycheck-add-mode 'typescript-tslint 'web-mode)
(flycheck-add-mode 'typescript-tide 'web-mode)


;; Format document before saving with the right options
;; More options here: https://github.com/Microsoft/TypeScript/blob/cc58e2d7eb144f0b2ff89e6a6685fb4deaa24fde/src/server/protocol.d.ts#L421-473

(setq tide-format-options
	  '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t	:placeOpenBraceOnNewLineForFunctions nil :tabSize 4 :convertTabsToSpaces nil))

(setq tide-user-preferences
	  '(:quotePreference "single"
		:includeCompletionsForModuleExports t
		:includeCompletionsWithInsertText t))

(add-hook 'before-save-hook 'tide-format-before-save)

;; Keybindings for tide

(defun tide-set-keys () 
  (local-set-key (kbd "C-c C-t r") 'tide-rename-symbol)
  (local-set-key (kbd "C-c C-t s") 'tide-restart-server)
  (local-set-key (kbd "C-c C-t f") 'tide-references)
  (local-set-key (kbd "C-c C-t e") 'tide-project-errors)
  (local-set-key (kbd "C-c C-t x") 'tide-fix)
  (local-set-key (kbd "M-RET") 'tide-fix))

(add-hook 'typescript-mode-hook 'tide-set-keys)
(add-hook 'web-mode-hook 'tide-set-keys)

;; Detect compilation errors in compile-mode
(add-to-list 'compilation-error-regexp-alist '("ERROR in \\(.*\\)(\\([0-9]+\\),\\([0-9]+\\)):$" 1 2))
