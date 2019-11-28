;; Utility funcs for agora dev


;; Base folders for restaurant and retail development
(setq ag:rest-base-dir "e:/desarrollo/igt-pos-rest/")
(setq ag:retail-base-dir "e:/desarrollo/igt-pos-retail/")


(defun _ag:start-4-shells (tl-title tl-path tr-title tr-path bl-title bl-path br-title br-path)
  "Splits frame in 4 windows and starts a shell in each one using specified title and path for each one (tl = top-left ... br = bottom-right)"

  (split-window-below)
  (split-window-right)
  (windmove-down)
  (split-window-right)
  
  (let ((default-directory br-path))
	(shell br-title))

  (windmove-left)

  (let ((default-directory tr-path))
	(shell tr-title))
  
  (windmove-down)
  
  (let ((default-directory tl-path))
	(shell tl-title))

  (windmove-right)
  
  (let ((default-directory bl-path))
	(shell bl-title))

  (windmove-up))

(defun ag:doomline-rest ()
  "Set powerline colors for Ágora Restaurant"
  (interactive)
  (set-face-attribute 'mode-line nil :background "light sky blue")
  (set-face-attribute 'mode-line-inactive nil :background "DodgerBlue4"))

(defun ag:doomline-retail ()
  "Set powerline colors for Ágora Retail"
  (interactive)
  (set-face-attribute 'mode-line nil :background "plum")
  (set-face-attribute 'mode-line-inactive nil :background "maroon4"))

(defun ag:start-rest ()
  "Setup shells for Ágora Restaurant"
  (interactive)
  (_ag:start-4-shells
   "rest:server" (concat (file-name-as-directory ag:rest-base-dir) "build/debug")
   "rest:web-admin" (concat (file-name-as-directory ag:rest-base-dir) "src/WebAdmin") 
   "rest:client" (concat (file-name-as-directory ag:rest-base-dir) "build/debug")
   "rest:aux" (concat (file-name-as-directory ag:rest-base-dir) ""))
  (ag:doomline-rest))

(defun ag:start-retail ()
  "Setup shells for Ágora Retail"
  (interactive)
  (_ag:start-4-shells
   "retail:server" (concat (file-name-as-directory ag:retail-base-dir) "build/debug")
   "retail:web-admin" (concat (file-name-as-directory ag:retail-base-dir) "src/WebAdmin") 
   "retail:client" (concat (file-name-as-directory ag:retail-base-dir) "build/debug")
   "retail:aux" (concat (file-name-as-directory ag:retail-base-dir) ""))
  (ag:doomline-retail))

(defun ag:admin-build ()
  "Starts a shell to build web admin"
  (interactive)
  (shell "*build webadmin*")
  (compilation-shell-minor-mode)
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer nil t)
  (toggle-truncate-lines)
  (insert "npm run dev")
  (comint-send-input nil t))


