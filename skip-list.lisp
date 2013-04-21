
(defparameter *a-skip-list* '(value next skip))

(defun skip-list (value)
	`(,value nil nil))

(defun get-next (slist)
	(cdar slist))

(defun get-skip (slist)
	(cddar slist))

(defun get-value (slist)
	(car slist))

;(defun set-next (next-value old-next)
;	(let ((skip (get-skip old-next)))
;		 `(,next-value ,old-next nil)))

(defun skip-cons (value tail)
	(list value tail nil))
(defun skip-car (slist)
	(car slist))
(defun skip-cdr (slist)
	(cdar slist))

(defun insert (value slist)
	(cond ((< value (get-value slist))
		   `(,value ,slist nil))
		  (t
		   `())