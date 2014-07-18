;; This is where your customizations should live

;; env PATH. Adds ~/.emacs.d/bin to the path when usin windows to
;; gain access to bundled unix tools (find, grep, etc.) 
(defun set-exec-path-from-shell-PATH ()
  (let* ((path-from-shell (getenv "PATH"))
		 (new-path (if (eq system-type 'windows-nt)					   
					   (let* ((bin (concat (expand-file-name user-emacs-directory) "bin\\")))
						 (concat bin path-separator path-from-shell))
					 path-from-shell)))
    (setenv "PATH" new-path)
    (setq exec-path (split-string new-path path-separator))))

;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it

;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 20) (height . 20)))


;; Place downloaded elisp files in this directory. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
;; Uncomment this to increase font size
;; (set-face-attribute 'default nil :height 140)

;; Flyspell often slows down editing so it's turned off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

(load "~/.emacs.d/vendor/clojure")
(load "~/.emacs.d/vendor/javascript")

;; hippie expand - don't try to complete with file names
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name hippie-expand-try-functions-list))
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name-partially hippie-expand-try-functions-list))

(setq ido-use-filename-at-point nil)

;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; Zenburn theme
(load-theme 'zenburn t)

;; Set UTF-8 as default encoding
(set-language-environment "UTF-8")

;; Add global key binding to fiplr-find-file
(global-set-key (kbd "C-x p") 'fiplr-find-file)

;; Guess indent for html files
(add-hook 'html-mode-hook 'sgml-guess-indent)

;; Set path
(set-exec-path-from-shell-PATH)

;; Show line numbers
(global-linum-mode t)

;; Load dirtree
(require 'tree-mode)
(require 'windata)
(require 'dirtree)
(global-set-key (kbd "C-o") 'dirtree-show)
