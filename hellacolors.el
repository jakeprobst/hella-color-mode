(defconst +colors+ '("#80FF00" "#FF4040" "#90a0f0" "#e04099" "#a0a080" "#88ff88"
                     "#C04000" "#C08040" "#FF8080" "#FFC0C0" "#FFFF80" "#40C000"
                     "#40C0C0" "#00C080" "#80C040" "#00FFC0" "#C0FF40" "#80FFFF"
                     "#a0a0ff" "#bfda44" "#bdafeb" "#bdefab" "#C080C0" "#dd4088"
                     "#ffc000" "#ffff80" "#c08080" "#808080" "#bbbbbb" "#9099ff"
                     "#e0e0e0"))

(defgroup hella-color nil
  "Options for hella colors"
  :prefix "hella-colors")

(defcustom hella-color-regex "[a-zA-Z0-9\-\+&]+"
  "Regex used for matching words"
  :group 'hella-color
  :type 'string)

(defvar hella-color-font-lock-keywords
  '(("[a-zA-Z0-9\-\+&]+" 0 (hella-color-match)))
  "font-lock to color individual words")

;; ripped from my python implementation
;; make a more lispy algorithm sometime?
(defun hella-hash (word)
  (let ((out 0)
	(iter 0))
    (mapcar
     (lambda (c)
       (cond ((eq (mod iter 4) 0) (setq out (+ out c)))
	     ((eq (mod iter 4) 1) (setq out (- out c)))
	     ((eq (mod iter 4) 2) (setq out (* out c)))
	     ((eq (mod iter 4) 3) (setq out (/ out c))))
       (setq iter (+ iter 1)))
     word)
    (mod out (length +colors+))))

(defun hella-get-color (word)
  (elt +colors+ (hella-hash word)))

(defun hella-color-match (&optional match)
  (unless match
    (setq match 0))
  (message (format "%s %d %d" (match-string match) (match-beginning match) (match-end match)))
  `((:foreground ,(hella-get-color (match-string-no-properties match)))))

(defun hella-color-mode-on ()
  ;(setq font-lock-keywords-only t) 
  (font-lock-add-keywords nil hella-color-font-lock-keywords))

(defun hella-color-mode-off ()
  (font-lock-remove-keywords nil hella-color-font-lock-keywords))
  
(define-minor-mode hella-color-mode
  "color words hella cool like"
  :init-value nil
  :lighter " Hella"
  :group hella-color
  (progn
    (if hella-color-mode
	(hella-color-mode-on)
      (hella-color-mode-off))
    (font-lock-mode 1)))

(provide 'hella-color-mode)
