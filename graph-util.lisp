
(defun edges->dot (edges)
	(mapc (lambda (node)
		(mapc (lambda (edge)
			(fresh-line)
			(princ (dot-name (car node)))
			(princ "->")
			(princ (dot-name (car edge)))
			(princ "[ label =\"")
			(princ (dot-label (cdr edge)))
			(princ "\"];"))
		(cdr node)))
	edges))

(defun dot->png (fname thunk)
	(with-open-file (* standard-output*
		fname
		:direction :output
		:if-exists :supersede)
	(funcall thunk))
	(ext:shell (concatenate 'string "dot -Tpng -O " fname)))

(defun graph->png (fname nodes edges)
	(dot-> png fname
		(lambda ()
			(graph-> dot nodes edges))))



(defun uedges->dot (edges)
	(maplist (lambda (lst)
		(mapc (lambda (edge)
			(unless (assoc (car edge) (cdr lst))
				(fresh-line)
				(princ (dot-name (caar lst)))
				(princ "--")
				(princ (dot-name (car edge)))
				(princ "[ label =\"")
				(princ (dot-label (cdr edge)))
				(princ "\"];")))
		(cdar lst)))
	edges))

(defun ugraph->dot (nodes edges)
	(princ "graph{")
	(nodes-> dot nodes)
	(uedges-> dot edges)
	(princ "}"))

(defun ugraph->png (fname nodes edges)
	(dot->png fname
		(lambda ()
			(ugraph-> dot nodes edges))))

