(require 'slime)

(setq inferior-lisp-program
	  "c:/utils/clisp/full/lisp.exe -B c:/utils/clisp/full -M c:/utils/clisp/full/lispinit.mem -ansi -q")

(slime-setup)
