

(defun guess-my-number ()
	(ash (+ *big* *small*) -1))

(defun smaller ()
	(setf *big* (1- (guess-my-number)))
	(guess-my-number))

(defun bigger ()
	(setf *small* (1+ (guess-my-number)))
	(guess-my-number))

(defun you-got-it ()
	"Hooray I got it! Try (start-over) to play again!")

(defun start-over ()
	(defparameter *small* 1)
	(defparameter *big* 100)
	(guess-my-number))
