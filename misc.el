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

(defun start-agora-shells ()
  "Splits frame in 4 windows and starts a shell in each one"

  (interactive)
  
  (split-window-below)
  (split-window-right)
  (windmove-down)
  (split-window-right)

  ;; top left
  (windmove-up)
  (let ((default-directory "e:/desarrollo/igt.pos-devel/build/debug"))
	(shell "Rest - Server"))

  ;; top right
  (windmove-right)
  (let ((default-directory "e:/desarrollo/igt.pos-devel/build/debug"))
	(shell "Rest - Client"))

  ;; bottom right
  (windmove-down)
  (let ((default-directory "e:/desarrollo/igt.pos.retail-devel/build/debug"))
	(shell "Retail - Client"))

  ;; top right
  (windmove-left)
  (let ((default-directory "e:/desarrollo/igt.pos.retail-devel/build/debug"))
	(shell "Retail- Server")))
