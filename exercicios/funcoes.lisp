;;  1.3
(defun findmax(el1 el2)
	(if (> el1 el2)
	    el1
	    el2
	)
)

;; 1.4
;; 1.4.1, findmax3 usando IF's
(defun findmax3-if(el1 el2 el3)
	(let ((mx)) 			;;define variavel mx
		(setf mx el1)           ;;atribui a mx o valor de el1
		(if (< mx el2)          
		    (setf mx el2)       ;;mx passa a ter o valor de el2, caso a condiçao se verifique
		    ()			;;em caso falso, retorna NIL
		)
		(if (< mx el3)
		    (setf mx el3)
		    () 			;;usar macro WHEN em vez de IF, para nao ter de escrever esta linha
		)
		mx               		;;retorna mx
	)
)

;; 1.4.2, findmax3 usando COND's
(defun findmax3-cond(el1 el2 el3)
	(let ((mx))
		(setf mx el1)
		(cond ((< mx el2)(setf mx el2))
		)
		(cond ((< mx el3)(setf mx el3))
		)
		mx
	)
)

;; 1.4.2, findmax3 usando 1.3
(defun findmax3-call(el1 el2 el3)
	(let ((mx))
		(setf mx (findmax el1 el2))
		(when (< mx el3)
			(setf mx el3)
		)
		mx
	)
)

;; 1.5
;; (defun soma(el1 el2)
;; 	(if(zerop el2)
;; 	   nil
;; 	   ( 
;; 	   )
;; 	)
;; )

;; 1.6
(defun potencia(base expoente)
	(if (= expoente 1)
	    base
	    (* base (potencia base (1- expoente))) ;; base * potencia(base,expoente-1)
	)
)

;; 1.8
(defun soma-1(lista)
	(if (null lista)
	    ()
	    (cons (1+ (first lista)) (soma-1 (rest lista)))
	)
)

;; 1.9
(defun soma-n(n lista)
	(if (null lista)
	    ()
	    (cons (+ n (first lista)) (soma-n n (rest lista)))
	)
)

;; 1.11
(cons '(1 2 3) '((3 5) (8)))

;; 1.12
(defun comprimento(lst)
	(let ((comp 0))
		(loop for x in lst
			do (incf comp)
		)
	comp
	)
)

;;1.13
(defun junta-obvio(lst1 lst2)
	(append lst1 lst2) 
)

(defun junta-recurs (lst1 lst2)
	(if (null lst1)
		lst2
		(cons (first lst1) (junta-recurs (rest lst1) lst2))
	)
)

;;1.14
(defun inverte(lst1)
	(if (null lst1)
		()
		(append (inverte (rest lst1)) (list (first lst1)))
	)
)

;;1.15
(defun list-contains(lst elem)
	(if (null lst)
		()
		(if (eq (first lst) elem)
			t
			(list-contains (rest lst) elem)
		)
	)
)

;;1.16
(defun list-remove(elem lst)
	(if (null lst)
		()
		(if (eq elem (first lst))
			(list-remove elem (rest lst))
			(cons (first lst) (list-remove elem (rest lst)))
		)
	)
)

;;1.17
(defun list-count (lst elem)
	(let ((n 0))
		(dolist (var lst)
			(when (eq var elem)
				(incf n)
			)
		)
		n
	)

)

;;1.25
(defun mapeia-obvio (predicate lst)
	(mapcar predicate lst)
)

(defun mapeia(predicate lst)
	(if (null lst)
		()
		(cons (funcall predicate (first lst)) (mapeia predicate (rest lst)))
	)
)

;; (mapeia #'1+ '(1 2 3))

;;1.26
(defun remove-se (predicate lst)
	(if (null lst)
		()
		(if (funcall predicate (first lst))
			(remove-se predicate (rest lst))
			(cons (first lst) (remove-se predicate (rest lst)))
		)
	)
)
















































