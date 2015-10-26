(defun potencia(base exponent)
	(if (= exponent 1)
		base
		(* base (potencia base (1- potencia)) 
	)
)