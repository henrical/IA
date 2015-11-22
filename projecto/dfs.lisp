;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPLEMENTATION OF DEPTH FIRST SEARCH
;;
;;


;;###################################################          
;;################## STACK (pilha) ##################
;;###################################################
;; STACK structure that allows push and pop 
;; operations.
;;
;; Attributes: 
;; - THIS: the stack itself. A list that contains the
;;   the elements of the stack.
;;
;; - POINTER: an integer that points to the top of 
;; the stack.
;;
(defstruct stack 
				(this '())
				(pointer 0)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CREATE-STACK
;; Returns an empty stack.
;;
(defun create-stack ()
	(make-stack)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STACK-EMPTY-P 
;; Returns true if stack is empty.
;;
(defun stack-empty-p (stack)
	(if (= (stack-pointer stack) 0)
		t
		nil
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STACK-PUSH!
;; Pushes an element onto the top of the stack.
;;
(defun stack-push! (stack elem)
	(if (stack-empty-p stack)
		(progn
			(setf (stack-this stack) (list elem))
			(incf (stack-pointer stack))
		)
		(progn
			(setf (stack-this stack) (cons elem (stack-this stack)))
			(incf (stack-pointer stack))
		)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STACK-POP!
;; Returns the element at the top of the stack.
;;
(defun stack-pop! (stack)
	(let ((result))
		(if (stack-empty-p stack)
			(setf result nil)
			(progn
				(setf result (first (stack-this stack)))
				(setf (stack-this stack) (rest (stack-this stack)))
				(decf (stack-pointer stack))
			)
		)
		
		;;return
		result
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CREATE-STACK-FROM-LIST
;; Returns a stack containing the elements of a given list.
;;
(defun create-stack-from-list (lst)
	(let ((result (create-stack)))
		(setf (stack-this result) lst)
		(setf (stack-pointer result) (list-length lst))
		
		result
	)
)

;;#####################################################         
;;##################### ALGORITHM #####################
;;#####################################################
;; Implementation of the depth first search 
;; algorithm.
;; 
;; ==============================================
;; Parameters:
;; 
;; PROBLEMA
;;
;; Estrutura com os seguintes atributos.
;; 	-ESTADO-INICIAL: o estado inicial da 
;; 	procura. E inserido na stack no inicio e a 
;; 	procura é feita a partir dai.
;;
;;  -SOLUCAO(estado): funcao teste-objectivo.
;;  E usada em todos os nos para testarem se sao 
;;  solucao do problema.
;; 
;;  -ACCOES(estado): funcao que devolve uma lista
;;  com todas as accoes possiveis de efectuar a 
;;  partir do estado actual.
;;
;;  -RESULTADO(estado, accao): funcao que aplicada 
;;  a um estado, recebe uma accao e devolve um novo
;;  estado resultante de aplicar a accao ao estado 
;;  dado. 
;;
;;  -CUSTO-CAMINHO(estado): funcao que devolve o 
;;  numero maximo de pontos possiveis de ter sido 
;;  obtidos num dado estado.
;;  E o equivalente ao custo de caminho num dado 
;;  estado.
;;
;; Para chamar uma funcao de uma variavel PROBLEMA
;; "prob":
;; (funcall (problema-solucao prob) estado1)
;; ==============================================
;;
;; Devolve uma lista com todas as accoes efectuadas
;; desde o estado inicial ate ao estado objectivo.


(defun dfs (problema)
	(let ((stack-estados (create-stack)) ;;----------------------------------
		  (estado-actual) ;;-----------------------------------------
		  (lista-accoes-estado-actual ) ;;----------------------------
;; 		  (stack-accoes (create-stack)) ;;------------------------------------------
		  (caminho-resultado '())) ;;--------------------------------------
		  
		(stack-push! stack-estados (problema-estado-inicial problema)) ;;----
		
		(block main-loop ;;------------------------------------------
		
			(loop do ;;----------------------------------------------
			
				(when (stack-empty-p stack-estados) ;;-----------------------
					(setf caminho-resultado nil) ;;-----------------------
					(print "STACK EMPTY.")
					(return-from main-loop)
				) 
				
				(setf estado-actual (stack-pop! stack-estados))
				
				(if (funcall (problema-solucao problema) estado-actual) ;;--------------- Se o estado é solucao:
					
					(progn
						(print "ESTADO SOLUCAO.")
					
;; 						(if (stack-empty-p stack-accoes)
;; 							()
;; 							(setf caminho-resultado
;; 								(append caminho-resultado (list (stack-pop! stack-accoes))) ;; Poe a ultima accao efectuada
;; 							)														;; no caminho
;; 						)

						(if (null lista-accoes)
							()
							(setf caminho-resultado (append caminho-resultado (list (first lista-accoes))))
						)
						
						(return-from main-loop) ;;--------------------------------------- Termina o ciclo.
					)

					
					
					(progn ;;------------------------------------------------------------ Se o estado nao é solucao:
						
						(print "EXPANDE NO.")

;; 						(if (stack-empty-p stack-accoes)
;; 							()
;; 							(setf caminho-resultado
;; 									(append caminho-resultado (list (stack-pop! stack-accoes))) ;; Poe a ultima accao efectuada
;; 							)															;; no caminho
;; 						)

						(if (null lista-accoes)
							()
							(setf caminho-resultado (append caminho-resultado (list (first lista-accoes))))
						)
						
						
						
						(setf lista-accoes (funcall (problema-accoes problema) estado-actual))
						
						
						(dolist (accao lista-accoes)
							(stack-push! stack-estados (resultado estado-actual accao))  
						)
					)
					
				)
			
			)
			
		)
		
;; 		(print caminho-resultado)
;; 		estado-actual
		caminho-resultado
	)
)

