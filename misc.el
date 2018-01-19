;; Funciones varias de utilidad

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (save-some-buffers)
  (dolist (buf (buffer-list))
	(with-current-buffer buf
	  (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
		(revert-buffer t t t) )))
  (message "Refreshed open files."))


;; Stolen from spacemacs
;; https://github.com/syl20bnr/spacemacs/blob/master/layers/better-defaults/funcs.el
(defun backward-kill-word-or-region (&optional arg)
  "Calls `kill-region' when a region is active and
`backward-kill-word' otherwise. ARG is passed to
`backward-kill-word' if no region is active."
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))


(defun _ag:start-4-shells (tl-title tl-path tr-title tr-path bl-title bl-path br-title br-path)
  "Splits frame in 4 windows and starts a shell in each one using specified title and path for each one (tl = top-left ... br = bottom-right)"

  (interactive)
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
	(shell bl-title)))


(defun ag:start-rest ()
  "Setup shells for Ágora Restaurant"
  (interactive)
  (_ag:start-4-shells
   "rest:server" "e:/desarrollo/igt.pos-devel/build/debug/"
   "rest:web-admin" "e:/desarrollo/igt.pos-devel/src/WebAdmin/"
   "rest:client" "e:/desarrollo/igt.pos-devel/build/debug/"
   "rest:aux" "e:/desarrollo/igt.pos-devel/build/")
  (set-face-attribute 'powerline-active2 nil :background "DodgerBlue1")
  (set-face-attribute 'powerline-inactive2 nil :background "DodgerBlue4"))

(defun ag:start-retail ()
  "Setup shells for Ágora Retail"
  (interactive)
  (_ag:start-4-shells
   "retail:server" "e:/desarrollo/igt.pos.retail-devel/build/debug/"
   "retail:web-admin" "e:/desarrollo/igt.pos.retail-devel/src/WebAdmin/"
   "retail:client" "e:/desarrollo/igt.pos.retail-devel/build/debug/"
   "retail:aux" "e:/desarrollo/igt.pos.retail-devel/build/")
  (set-face-attribute 'powerline-active2 nil :background "maroon1")
  (set-face-attribute 'powerline-inactive2 nil :background "maroon4"))


(defun distraction-free ()
  "Creates a distraction free environment to edit the current buffer"
  (interactive)
  (modify-frame-parameters nil `((fullscreen . fullboth)))
  (delete-other-windows)

  ;; FIXME: This does only work if frame was already fullscreen
  (set-fringe-mode
   (/ (- (frame-pixel-width)
		 (* 80 (frame-char-width)))
	  2))

  (let ((bc (face-attribute 'default :background)))
	(custom-set-faces '(fringe ((t (:background "#2d2d2d")))))))
