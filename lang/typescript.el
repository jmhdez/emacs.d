(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            (company-mode-on)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;;(add-hook 'before-save-hook 'tide-format-before-save)

;; format options
(setq tide-format-options
	  '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t	 :placeOpenBraceOnNewLineForFunctions nil :tabSize 4 :convertTabsToSpaces nil))
;; see https://github.com/Microsoft/TypeScript/blob/cc58e2d7eb144f0b2ff89e6a6685fb4deaa24fde/src/server/protocol.d.ts#L421-473 for the full list available options

;; Tide can be used along with web-mode to edit tsx files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (member (file-name-extension buffer-file-name) '("ts" "tsx"))
              (tide-setup)
              (flycheck-mode +1)
              (setq flycheck-check-syntax-automatically '(save mode-enabled))
              (eldoc-mode +1)
              (company-mode-on))))


;; Keybindings for tide

(defun tide-set-keys () 
  (local-set-key (kbd "C-c C-t r") 'tide-rename-symbol)
  (local-set-key (kbd "C-c C-t s") 'tide-restart-server)
  (local-set-key (kbd "C-c C-t f") 'tide-references))


(add-hook 'typescript-mode-hook 'tide-set-keys)
(add-hook 'web-mode-hook 'tide-set-keys)

