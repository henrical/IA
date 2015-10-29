(defun make-counter1 ()
	(let ((i 0))
		#'(lambda (val)
			(incf val)
		)
	)

)

;; (setf counter1 (make-counter))
;; (funcall counter1 5) --> 6
;; (funcall counter1 6) --> 7

(defun make-counter2 (val)
	(let ((i val))
		#'(lambda ()
			(incf i)
		)
	)

)


;; (setf counter1 (make-counter 5))
;; (funcall counter1) --> 6
;; (funcall counter1) --> 7
