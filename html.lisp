(defparameter *html-stuff* '(html () (head () (title () "something"))
	(body ((margin 5) (background "Blue"))
		(h1 () "BIG TEXT")
		(h2 () "Not so big text"))))


(defun nil? (obj) (eq nil obj))
(defun string? (obj) (simple-string-p obj))


(defun list-to-html (html-list)
	(labels ((to-attributes (attr-list)
							(cond ((nil? attr-list) "")
								  (t (let ((attribute (car attr-list)))
								  		  (concatenate 'string
								  		  			   " "
								  		  			   (prin1-to-string (car attribute))
								  		  			   "="
								  		  			   (prin1-to-string (cadr attribute))
								  		  			   ""
								  		  			   (to-attributes (cdr attr-list))))))))
			(cond ((nil? html-list) "")
			  	  ((string? html-list) html-list)
			  	  (t (concatenate 'string
			  				  	  "<"
			  				  	  (prin1-to-string (car html-list))
			  				  	  (to-attributes (cadr html-list))
			  				  	  ">"
			  				  	  (apply #'concatenate (cons 'string (mapcar #'list-to-html (cddr html-list))))
			  				  	  "</"
			  				  	  (prin1-to-string (car html-list))
			  				  	  ">")))))
