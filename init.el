﻿(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; smex settings

(smex-initialize)

;; auto-complete settings

(require 'auto-complete-config) ; No sé por qué este require no lo añade package.el
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

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

;; visual settings: UI elements, theme, fonts, ...

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'sanityinc-tomorrow-eighties t)

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(blink-cursor-mode 0)
(show-paren-mode 1)

(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Consolas"))

;; some default values

(setq backup-directory-alist `(("." . "~/.saves")))
(set-language-environment "UTF-8")
(global-linum-mode t)

(setq-default indents-tab-mode t)
(setq-default tab-width 4)

(global-visual-line-mode t)

  ;; Use buffer name and fullpath as window title
(setq-default frame-title-format "%b (%f)")
  ;; Changes all yes/no questions to y/n
(fset 'yes-or-no-p 'y-or-n-p)

  ;; Ensure buffer names are unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;; utility functions
(load "~/.emacs.d/misc.el");

;; language customizations

(load "~/.emacs.d/lang/javascript.el")
(load "~/.emacs.d/lang/clojure.el")
(load "~/.emacs.d/lang/html.el")
(load "~/.emacs.d/lang/haskell.el")
(load "~/.emacs.d/lang/octave.el")

;; use paredit in any lisp-like mode

(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)


;; key bindings

(load "~/.emacs.d/key-bindings.el")
