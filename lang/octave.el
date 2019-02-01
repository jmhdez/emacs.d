;; octave

;; Since I'm not going to use Objective-C in a near future,
;; let octave-mode take over .m files

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
