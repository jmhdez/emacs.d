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

(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

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

;; Git Bash
(defun git-bash () 
  (interactive)
  (let ((explicit-shell-file-name "C:/Program Files/git/bin/bash.exe"))
    (call-interactively 'shell)
    (insert "export PS1='\\[\\033[32m\\]\\[\\033[33m\\]\\w\\[\\033[36m\\]`__git_ps1`\\[\\033[0m\\]\\n$ '")
    (comint-send-input nil t)
	(rename-buffer "*git*")))
