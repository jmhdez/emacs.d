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
