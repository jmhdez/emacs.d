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


(defun start-agora-shells ()
  "Splits frame in 4 windows and starts a shell in each one"

  (interactive)
  
  (split-window-below)
  (split-window-right)
  (windmove-down)
  (split-window-right)

  ;; top left
  (windmove-up)
  (let ((default-directory "e:/desarrollo/igt.pos-devel/build/debug/"))
	(shell "Rest - Server"))

  ;; top right
  (windmove-right)
  (let ((default-directory "e:/desarrollo/igt.pos-devel/build/debug/"))
	(shell "Rest - Client"))

  ;; bottom right
  (windmove-down)
  (let ((default-directory "e:/desarrollo/igt.pos.retail-devel/build/debug/"))
	(shell "Retail - Client"))

  ;; top right
  (windmove-left)
  (let ((default-directory "e:/desarrollo/igt.pos.retail-devel/build/debug/"))
	(shell "Retail- Server")))

(defun distraction-free ()
  "Creates a distraction free environment to edit the current buffer"

  (interactive)

  (modify-frame-parameters nil `((fullscreen . fullboth)))
  
  (delete-other-windows)
  (split-window-right)
  (split-window-right)

  (let* ((original (current-buffer))
		 (total (frame-width))
		 (center 80)
		 (left (/ (- total center) 2))
		 (right (- total center left)))

	;; load a blank buffer in left-window
	(get-buffer-create "left-padding")
	(switch-to-buffer "left-padding")
	(adjust-window-trailing-edge (selected-window) (- left (window-width)) t)

	;; load original buffer in center-window
	(windmove-right)
	(switch-to-buffer original)
	(adjust-window-trailing-edge (selected-window) (- center (window-width)) t)

	;; load a blank buffer in right-window (size is already set)
	(windmove-right)
	(get-buffer-create "right-padding")
	(switch-to-buffer "right-padding")

	;; return to center-window
		(windmove-left)))
