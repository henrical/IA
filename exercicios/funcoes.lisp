;;  1.3
(defun findmax(el1 el2)
	(if (> el1 el2)
	      (+ 0 el1)
		(+ 0 el2)
	)
)

;; 1.4
;; 1.4.1, findmax3 usando IF's
(defun findmax3(el1 el2 el3)
	(if (> el1 el2 el3)
		(+ 0 el1)
		(
			(if (> el1 el3 el2)
				(+ 0 el1)
			)
		)
	)
)