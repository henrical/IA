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



















































