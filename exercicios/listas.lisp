;;Função inverte lista
(defun invert-lst (lst)
	"inverts a list"
	(if (null lst)
		()
		(append (invert-lst (rest lst)) (list (first lst)))
	)
)

;;Substitui ocorrencias do 1º elemento pelo 2º na lista
(defun subs (elem1 elem2 lst)
	(cond (
			(null lst)
			()
		)
		
		(
			(equal (first lst) elem1)
			(cons elem2 (subs elem1 elem2 (rest lst)))
		)
		
		(
			t
			(cons (first lst) (subs elem1 elem2 (rest lst)))
		)
	)
)

;;Incrementa valores de uma lista
;;Incremento é opcional. Se não for passado, o valor default é 1.
;;--Para chamar, usar "(inc-list '(lista) :inc 5)
(defun inc-list (lst &key (inc 1))
	(if (null lst)
		()
		(cons (+ inc (first lst)) (inc-list (rest lst) :inc inc))
	)
)

;;Conta occorrencias de um elemento numa lista
;;com 'dolist'
(defun list-contains1 (lst elem)
	(let ((n 0))
		(dolist (var lst)
			(when (eq var elem)
				(incf n)
			)
		)
		n
	)

)

;;com 'loop,do'
(defun list-contains2 (lst elem)
	(let ((n 0))
		(loop for var in lst
			do 
			(when (eq var elem)
				(incf n)
			)
				
		)
		n
	)


)