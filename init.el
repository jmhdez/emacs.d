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
(add-to-list 'projectile-globally-ignored-directories "*node_modules*")
(add-to-list 'projectile-globally-ignored-directories "node_modules")
(setq projectile-indexing-method 'alien)
;; I don't why this is stopped working with default command in windows. I have added
;; the removal of \r and it works again.
(setq projectile-svn-command "svn list -R . | grep -v '$/' | tr -d '\\r' | tr '\\n' '\\0")

;; visual settings: UI elements, theme, fonts, ...

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq-default truncate-lines t
			  truncate-partial-width-windows t)
(set-display-table-slot standard-display-table 0 ?\…)

(setq inhibit-startup-message t)
(setq-default cursor-in-non-selected-windows nil)

(setq-default cursor-type 'bar)
(blink-cursor-mode 0)
(show-paren-mode 1)

;; theme (doom w/ doom-modeline)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

(setq doom-themes-enable-bold nil
	  doom-themes-enable-italic t)

;; fine-tunnig spacegrey theme for neotree and highlight colors
(load-theme 'doom-spacegrey t)

(with-eval-after-load 'neotree
  (set-face-attribute 'neo-dir-link-face nil :foreground "#8FA1B3")) ;; spacegrey blue color
(set-face-attribute 'highlight nil :foreground "#D08770") ; orange
(set-face-attribute 'highlight nil :background "#232830") ; bg-alt
(set-face-attribute 'highlight nil :underline t)


(doom-themes-neotree-config)
(doom-modeline-mode 1)
(setq doom-modeline-height 28)
(setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-bar-width 5)
(setq doom-modeline-major-mode-icon nil)
(setq doom-modeline-minor-modes t)
(setq doom-modeline-buffer-file-name-style 'buffer-name)
(minions-mode 1)

;; Use differents backgrounds for code and tools 
(solaire-global-mode +1)
(add-hook 'after-revert-hook #'turn-on-solaire-mode)
(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
(solaire-mode-swap-bg)

;; Show line numbers
(global-display-line-numbers-mode)
(setq-default display-line-numbers-width 3)
;; Disable line-numbers minor mode for neotree
(add-hook 'neo-after-create-hook
          (lambda (&rest _) (display-line-numbers-mode -1)))

;; Fonts
(let ((font-family "Fira Code"))
  (when (member font-family (font-family-list))
	(setq-default line-spacing 4)
	(set-face-attribute 'default nil :font font-family)
	(set-face-attribute 'default nil :height 110)
	(set-face-attribute 'default nil :width 'normal)
	(set-face-attribute 'default nil :slant 'normal)
	(set-face-attribute 'default nil :weight 'regular)))


(add-to-list 'default-frame-alist '(left-fringe . 0))
(add-to-list 'default-frame-alist '(right-fringe . 0))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(delete-selection-mode t)

(setq-default indents-tab-mode t)
(setq-default tab-width 4)

(global-hl-line-mode t)

;; neotree
(setq neo-window-fixed-size nil)
(setq neo-window-width 35)
;; show current file when opened
(setq neo-smart-open t)
(setq neo-hide-cursor t)
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
(load "~/.emacs.d/lang/clisp.el")

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
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
