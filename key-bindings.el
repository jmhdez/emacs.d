;; Disable C-x C-c as save-buffers-kill-terminal
(global-unset-key (kbd "C-x C-c"))

;; Completion on M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; It's all about the project.
(global-set-key (kbd "C-x p") 'helm-projectile)

;; Use helm instead for some common operations
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-<insert>") 'helm-show-kill-ring)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Search, replace and code navigation
(global-set-key (kbd "C-x h") 'replace-string)
(global-set-key (kbd "C-x C-h") 'rgrep)
;; Jump to a definition in the current file. (Protip: this is awesome.)
(global-set-key (kbd "C-x C-i") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-x C-o") 'helm-occur)
;; I barely use C-x o to switch buffers, and I'm always getting the helm-occur keybinding wrong so..
(global-set-key (kbd "C-x o") 'helm-occur)

;; Multicursor editing
(global-set-key (kbd "C-S-d") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C-'") 'mc-hide-unmatched-lines-mode)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x C-m") 'shell)

;; If you want to be able to M-x without meta (phones, etc)
(global-set-key (kbd "C-c x") 'execute-extended-command)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Reload all buffers from disk
(global-set-key (kbd "C-c C-r") 'revert-all-buffers)

;; By default, indent on new line
(global-set-key (kbd "RET") 'newline-and-indent)

;; Set undo to a sane shortcut
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

;; Make C-<backspace> delete word or region
(global-set-key (kbd "C-<backspace>") 'backward-kill-word-or-region)

;; Toggle hide/show block with HideShow minor mode
(global-set-key (kbd "C-c C-t") 'hs-toggle-hiding)

;; Navigation with avy
(global-set-key (kbd "M-s") 'avy-goto-word-1)

;; Toggle neotree using custom function to follow projectile project root
(global-set-key [f8] 'neotree-project-dir)
;; find file in neotree
(global-set-key (kbd "M-L") (lambda () (interactive) (neotree-find (buffer-file-name))))
