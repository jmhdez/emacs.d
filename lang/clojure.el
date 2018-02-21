;; clojure

(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))

;; cider
(setq cider-repl-pop-to-buffer-on-connect t)
(setq cider-popup-stacktraces t)
(setq cider-repl-popup-stacktraces t)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-history-file "~/.emacs.d/cider-history")
(setq cider-repl-wrap-history t)

(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(defun enable-clj-refactor ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'enable-clj-refactor)

(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'cider-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'auto-complete-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
		  
(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")
