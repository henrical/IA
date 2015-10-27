(defun make-counter ()
	(let ((i 0))
		#'(lambda ()
			(incf i)
		)
	)

)

;; (setf counter1 (make-counter))
;; (funcall counter1)
;; (funcall counter1)
