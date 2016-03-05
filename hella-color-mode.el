;;; hella-color-mode.el --- color all words in a document

;;; Commentary:
;;; I have found that having all my variables/functions be different colors
;;; helps me read code.
;;; See https://medium.com/@evnbr/coding-in-color-3a6db2743a1e for the article
;;; that made me decide to try it out.
;;; I do deviate a bit from the idea and just color everything.

;;; Code:
(defconst +hella-color-colors+ '("#80FF00" "#FF4040" "#90a0f0" "#e04099" "#a0a080" "#88ff88"
				 "#C04000" "#C08040" "#FF8080" "#FFC0C0" "#FFFF80" "#40C000"
				 "#40C0C0" "#00C080" "#80C040" "#00FFC0" "#C0FF40" "#80FFFF"
				 "#a0a0ff" "#bfda44" "#bdafeb" "#bdefab" "#C080C0" "#dd4088"
				 "#ffc000" "#ffff80" "#c08080" "#808080" "#bbbbbb" "#9099ff"
				 "#e0e0e0"))

(defconst +hella-color-default-regex+ "[a-zA-Z0-9\-\+&_/!<>\?\\#:]+")
(defvar hella-color-regex nil)

;; ripped from my python implementation
;; make a more lispy algorithm sometime?
(defun hella-hash (word)
  "Hash a WORD to find its index in the color list."
  (let ((out 0)
	(iter 0))
    (mapc
     (lambda (c)
       (cond ((eq (mod iter 4) 0) (setq out (+ out c)))
	     ((eq (mod iter 4) 1) (setq out (- out c)))
	     ((eq (mod iter 4) 2) (setq out (* out c)))
	     ((eq (mod iter 4) 3) (setq out (/ out c))))
       (setq iter (+ iter 1)))
     word)
    (mod out (length +hella-color-colors+))))

(defun hella-get-color (word)
  "Fetch the color for the corresponding WORD."
  (elt +hella-color-colors+ (hella-hash word)))

(defun hella-color-match (&optional match)
  "Get the color of a regex MATCH."
  (unless match
    (setq match 0))
  `((:foreground ,(hella-get-color (match-string-no-properties match)))))

(defun hella-color-remove-color (regex)
  "No longer hella color REGEX."
  (font-lock-remove-keywords nil (list `(,regex 0 (hella-color-match)))))

(defun hella-color-set-regex (regex)
  "Specify a REGEX to hella color."
  (when hella-color-regex
    (hella-color-remove-color hella-color-regex))
  (setq hella-color-regex regex)
  (font-lock-add-keywords nil (list `(,regex 0 (hella-color-match)))))

(defun hella-color-mode-on ()
  "Enable hella coloring."
  (setq font-lock-keywords '())
  (hella-color-set-regex +hella-color-default-regex+))

(defun hella-color-mode-off ()
  "Disable hella coloring."
  (hella-color-remove-color hella-color-regex))

(define-minor-mode hella-color-mode
  "color words hella cool like"
  :init-value nil
  :lighter " Hella"
  ;:group hella-color
  (progn
    (if hella-color-mode
	(hella-color-mode-on)
      (hella-color-mode-off))
    (font-lock-mode 1)))

(provide 'hella-color-mode)
;;; hella-color-mode ends here
