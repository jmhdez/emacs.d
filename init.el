(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; folder to save all temp files
(setq emacs-persistence-directory (concat user-emacs-directory "persistence/"))
(unless (file-exists-p emacs-persistence-directory)
  (make-directory emacs-persistence-directory t))

(setq ido-save-directory-list-file (concat emacs-persistence-directory "ido-last"))
(setq backup-directory-alist `(("." . ,(concat emacs-persistence-directory "saves"))))
(setq auto-save-file-name-transforms `((".*" ,(concat emacs-persistence-directory "auto-saves") t)))
(setq auto-save-list-file-prefix (concat emacs-persistence-directory "saves"))

(setq create-lockfiles nil)

;; smex settings

(smex-initialize)

;; ido settings

(ido-mode t) 
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;; helm

(setq helm-split-window-in-side-p t
	  helm-buffers-fuzzy-matching t
	  helm-recentf-fuzzy-match t)

;; projectile

(projectile-global-mode)
(add-to-list 'projectile-globally-ignored-directories "node_modules")
(setq projectile-indexing-method 'alien)
;; I don't why this is stopped working with default command in windows. I have added
;; the removal of \r and it works again.
(setq projectile-svn-command "svn list -R . | grep -v '$/' | tr -d '\\r' | tr '\\n' '\\0")

;; visual settings: UI elements, theme, fonts, ...

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq-default truncate-lines nil
			  truncate-partial-width-windows nil)

(setq inhibit-startup-message t)
(setq-default cursor-in-non-selected-windows nil)

(blink-cursor-mode 0)
(show-paren-mode 1)

;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(setq solarized-use-less-bold t)
(setq solarized-high-contrast-mode-line t)
(load-theme 'solarized-dark t)
(custom-theme-set-faces
 'solarized-dark
 '(helm-selection ((t (:background "#073642" :foreground "#b58900")))))

;; linum-mode (face matches current theme)
(global-linum-mode t)
(setq linum-format "%3d")
(set-face-attribute 'linum nil :foreground "#23454F")

;; powerline settings
(setq powerline-default-separator 'brace)
(powerline-center-theme)

(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)


;; Fonts
(let ((font-family "Hack"))
  (when (member font-family (font-family-list))
	(setq-default line-spacing 5)
	(set-face-attribute 'default nil :font font-family)
	(set-face-attribute 'default nil :height 105)))


(add-to-list 'default-frame-alist '(left-fringe . 4))
(add-to-list 'default-frame-alist '(right-fringe . 0))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(delete-selection-mode t)

(setq-default indents-tab-mode t)
(setq-default tab-width 4)

;; Do NOT truncate lines. I'm sure this is not the recommended way
;; to do it, but I don't know how to make it work
(add-hook 'find-file-hook (lambda () (toggle-truncate-lines t)))

(global-hl-line-mode t)



;; neotree
(setq neo-theme 'ascii)
(setq neo-window-width 35)
;; show current file when opened
(setq neo-smart-open t) 
;; set neotree root to projectile root
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
		(file-name (buffer-file-name)))
	(neotree-toggle)
	(if project-dir
		(if (neo-global--window-exists-p)
			(progn
			  (neotree-dir project-dir)
			  (neotree-find file-name)))
	  (message "Could not find git project root."))))


;; undo-tree
(global-undo-tree-mode t)

;; Default values 

(set-language-environment "UTF-8")

;; Use buffer name and fullpath as window title
(setq-default frame-title-format
			  '((:eval (if (buffer-file-name) "%b -- %f" "%b"))))

;; Changes all yes/no questions to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Ensure buffer names are unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Enable global-auto-revert-mode
(global-auto-revert-mode t)

;; utility functions
(load "~/.emacs.d/misc.el");
(load "~/.emacs.d/agora.el")

;; language customizations

(load "~/.emacs.d/lang/javascript.el")
(load "~/.emacs.d/lang/clojure.el")
(load "~/.emacs.d/lang/html.el")
(load "~/.emacs.d/lang/haskell.el")
(load "~/.emacs.d/lang/octave.el")
(load "~/.emacs.d/lang/typescript.el")

;; use paredit in any lisp-like mode

(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)

;; company mode for autocompletion
(add-hook 'after-init-hook 'global-company-mode)
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
(setq company-idle-delay 0.5)


;; Windows hacks.

;; Autocomplete on shell buffers with \ instead of /
(defun win-file-name-completion-advice (res)
  (if (stringp res) (replace-regexp-in-string "/" "\\\\" res) res))

(defun win-command-completion-advice ()
  (let ((filename (comint-match-partial-filename)))
    (and filename (not (string-match "\\\\" filename)))))


(when (eq system-type 'windows-nt)
  (advice-add 'comint-completion-file-name-table
			  :filter-return #'win-file-name-completion-advice))

(when (eq system-type 'windows-nt)
  (advice-add 'shell-command-completion
			  :before-while #'win-command-completion-advice))


;; key bindings

(load "~/.emacs.d/key-bindings.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1ce793cf04c7fbb4648c20f079b687ef10d8ee3014422cf67cf08c92fa6dc77c" "9bc6cf0c6a6c4b06b929e8cd9952478fa0924a4c727dacbc80c3949fc0734fb9" default)))
 '(package-selected-packages
   (quote
	(markdown-mode+ yaml-mode magit rjsx-mode esup 0blayout twittering-mode solarized-theme undo-tree company-restclient restclient-helm web-mode uuidgen tide smex restclient rainbow-delimiters powerline neotree mocha less-css-mode js2-refactor ido-ubiquitous helm-projectile haskell-mode git-rebase-mode git-commit-mode flycheck-color-mode-line company color-theme-sanityinc-tomorrow clj-refactor avy auto-complete ac-js2))))


;; Configuración de fuentes para flycheck-color-mode-line
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-color-mode-line-error-face ((t (:foreground "#DC322F" :weight normal))))
 '(flycheck-color-mode-line-info-face ((t (:inherit flycheck-fringe-info :foreground "#2AA198" :weight normal))))
 '(flycheck-color-mode-line-warning-face ((t (:inherit flycheck-fringe-warning :background "#839496" :foreground "#CB4B16" :weight normal)))))
