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

(setq-default truncate-lines nil
			  truncate-partial-width-windows nil)

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
(load-theme 'doom-nord t)
(doom-themes-neotree-config)
(doom-modeline-mode 1)
(setq doom-modeline-height 28)
(setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-bar-width 3)
(setq doom-modeline-major-mode-icon nil)
(setq doom-modeline-minor-modes t)
(setq doom-modeline-buffer-file-name-style 'buffer-name)
(minions-mode 1)

;; linum-mode
(global-linum-mode t)
(setq linum-format "%3d")

;; Fonts
(let ((font-family "Fira Code"))
  (when (member font-family (font-family-list))
	(setq-default line-spacing 4)
	(set-face-attribute 'default nil :font font-family)
	(set-face-attribute 'default nil :height 110)
	(set-face-attribute 'default nil :width 'normal)
	(set-face-attribute 'default nil :slant 'normal)
	(set-face-attribute 'default nil :weight 'regular)))


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
(setq neo-window-fixed-size nil)
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
 '(ansi-color-names-vector
   ["#2E3440" "#BF616A" "#A3BE8C" "#EBCB8B" "#81A1C1" "#B48EAD" "#88C0D0" "#ECEFF4"])
 '(custom-safe-themes
   (quote
	("7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "9306dc41c37b6eca31fff5405af0dbb48eaabb3e4f26150b1c40cbcb3cce171a" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "392395ee6e6844aec5a76ca4f5c820b97119ddc5290f4e0f58b38c9748181e8d" "030bed79e98026124afd4ef8038ba7fe064314baf18b58759a5c92b91ec872fb" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1ce793cf04c7fbb4648c20f079b687ef10d8ee3014422cf67cf08c92fa6dc77c" "9bc6cf0c6a6c4b06b929e8cd9952478fa0924a4c727dacbc80c3949fc0734fb9" default)))
 '(fci-rule-color "#4C566A")
 '(jdee-db-active-breakpoint-face-colors (cons "#191C25" "#81A1C1"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#191C25" "#A3BE8C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#191C25" "#434C5E"))
 '(objed-cursor-color "#BF616A")
 '(package-selected-packages
   (quote
	(vc-msg slime php-mode minions powershell markdown-mode+ yaml-mode magit rjsx-mode esup 0blayout twittering-mode solarized-theme undo-tree company-restclient restclient-helm uuidgen smex restclient rainbow-delimiters neotree mocha less-css-mode js2-refactor ido-ubiquitous helm-projectile haskell-mode git-rebase-mode git-commit-mode flycheck-color-mode-line company color-theme-sanityinc-tomorrow clj-refactor avy auto-complete ac-js2)))
 '(pdf-view-midnight-colors (cons "#ECEFF4" "#2E3440"))
 '(rustic-ansi-faces
   ["#2E3440" "#BF616A" "#A3BE8C" "#EBCB8B" "#81A1C1" "#B48EAD" "#88C0D0" "#ECEFF4"])
 '(vc-annotate-background "#2E3440")
 '(vc-annotate-color-map
   (list
	(cons 20 "#A3BE8C")
	(cons 40 "#bbc28b")
	(cons 60 "#d3c68b")
	(cons 80 "#EBCB8B")
	(cons 100 "#e2b482")
	(cons 120 "#d99d79")
	(cons 140 "#D08770")
	(cons 160 "#c68984")
	(cons 180 "#bd8b98")
	(cons 200 "#B48EAD")
	(cons 220 "#b77f96")
	(cons 240 "#bb7080")
	(cons 260 "#BF616A")
	(cons 280 "#a05b67")
	(cons 300 "#815664")
	(cons 320 "#625161")
	(cons 340 "#4C566A")
	(cons 360 "#4C566A")))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
