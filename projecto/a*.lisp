;;###################################################          
;;################## ORDERED STACK ##################
;;###################################################
;; Stack that stores ordered elements.
;; 
;; Attributes:
;;  
;; - THIS: the stack itself. A list that contains the
;;   the elements of the stack.
;; 	 The list is ordered from lowest to highest value.
;; 
;; - POINTER: an integer that points to the top of 
;; the stack.
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CREATE-STACK
;; Returns an empty stack.
;;
;; (defun create-stack ()
;; 	(make-stack)
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STACK-EMPTY-P 
;; Returns true if stack is empty.
;;
;; (defun stack-empty-p (stack)
;; 	(if (= (stack-pointer stack) 0)
;; 		t
;; 		nil
;; 	)
;; )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STACK-ORDERED-PUSH!
;; Pushes ELEM onto the stack. Insertion order is:
;; 	- lowest value of VALUE at head of stack.
;; 	- highues value of VALUE at the end.
;;
;;  Values are determined by [custo(elem) + heuristica(elem)]
;;
(defun stack-ordered-push! (stack elem custo heuristica)
	(let ((new-elem-value (+ (funcall custo elem) (funcall heuristica elem)))
		  
		  (stack-pre-list '())
		  (stack-post-list (stack-this stack))
		  
		  (curr-elem (first (stack-this stack)))
		  
		  (result '())
		 )
		 (let ((curr-elem-value))
	
			(if (stack-empty-p stack)
				(progn
					(setf result (list elem))
				)
				(progn
		
					(block main-loop
						(loop do
					
							(when (null curr-elem)
								(setf result (append (reverse stack-pre-list) (list elem )))
								(return-from main-loop)
							)
					
							(setf curr-elem-value (+ (funcall custo curr-elem) (funcall heuristica curr-elem)))
					
;; 							(return-from main-loop stack-pre-list)
							
							
					
							(if (<= new-elem-value curr-elem-value)
								(progn
									(setf result (append (reverse stack-pre-list) (cons elem stack-post-list))) 
									(return-from main-loop)
								)
								(progn
									(setf stack-pre-list (cons curr-elem stack-pre-list))
									(setf stack-post-list (rest stack-post-list))
								
									(setf curr-elem (first stack-post-list))
								)
							)	
					
						
						)
					)
					
				)
				
			)
		
		(incf (stack-pointer stack))
		(setf (stack-this stack) result)
		t
		
		)
	)
)

(defun procura-A* (problema heuristica)
	
	;;################################ DECLARACOES DE VARIAVEIS ###############################################
	
	(let ((stack-estados (create-stack)) ;;-------------------------------- stack onde sao guardados os estados.
		  (estado-actual) ;;----------------------------------------------- estado a ser explorado na iteracao actual.
										                                  
		  (lista-accoes-estado-actual ) ;;---------------------------------  lista temporario para guardar todos as accoes 
																		  ;; possiveis de efectuar sobre um estado.
																		  ;; (retorno da funcao ACCOES(estado))
					
		  (mapa-estado-accao (make-hash-table)) ;;------------------------- hash table que mapeia um estado a accao que levou a ele
		  (mapa-estado-antecessor (make-hash-table))
	
		  (estado-resultado)			
					
		  (caminho-resultado '())) ;;-------------------------------------- guarda o caminho actual tomado (lista de accoes)
		  
		  
		  
	;;################################# FIM DE DECLARACOES ####################################################  
		 
		 
		(stack-ordered-push! stack-estados (problema-estado-inicial problema) (problema-custo-caminho problema) heuristica)
		
		(block main-loop
			
			(loop do ;;---------------------------------------------------- ciclo principal
;; 				(print "loop principal.")
				(when (stack-empty-p stack-estados) ;;--------------------- se encontrar a stack vazia, nao existe solucao.
					(setf caminho-resultado nil) ;;------------------------ poe o resultado a nulo
					(return-from main-loop) ;;----------------------------- sai do ciclo
				) 
				
				
				(setf estado-actual (stack-pop! stack-estados)) ;;--------- vai buscar o proximo estado a ser explorado a stack.
				
;; 				(print estado-actual)
				
;; 				(print (estado-pecas-por-colocar estado-actual))
				
				(if (funcall (problema-solucao problema) estado-actual) ;; testa se o estado actual e solucao
					
					;;--------------------------------------------------- se o estado e solucao:
					(progn
						(return-from main-loop) ;;----------------------- Termina o algoritmo e retorna o caminho.
					)
					
					
					;;--------------------------------------------------- se o estado nao e solucao:
					(progn 
;; 						(print "Estado nao é solucao.")
					
						;; determina a lista de accoes possiveis
						;; a partir do estado actual 
						(setf lista-accoes-estado-actual 
							(funcall (problema-accoes problema) estado-actual)
						)
						
;; 						(print "Stack:")
;; 						(print lista-accoes-estado-actual)
						
						;; expande o estado actual, adicionando todos os estados possiveis a stack.
						(dolist (accao lista-accoes-estado-actual) 
							(setf estado-resultado (resultado estado-actual accao))
						
							(stack-ordered-push! stack-estados estado-resultado (problema-custo-caminho problema) heuristica)
							
							(setf (gethash estado-resultado mapa-estado-accao) accao)
							(setf (gethash estado-resultado mapa-estado-antecessor) estado-actual)
							
						)
						
;; 						(read-char)
					)
					
				)
			
			)
			
		)
		
;; 		(print "determinacao caminho.")
		;;determinar caminho
		(block determinacao-caminho
		
			(loop do
				
				(when (null estado-actual)
					(return-from determinacao-caminho)
				)
				
				(when (not (null (gethash estado-actual mapa-estado-accao)))
					(setf caminho-resultado (cons (gethash estado-actual mapa-estado-accao) caminho-resultado))
				)
				
				(setf estado-actual (gethash estado-actual mapa-estado-antecessor))
			)
		
		)
	
		
		caminho-resultado ;;----------------------------------------------- retorna o caminho obtido.
																		 ;; e nil caso nao tenho encontrado solucao. 
	)
)
