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
		
		)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STACK-POP!
;; Returns the element at the top of the stack.
;;
;; (defun stack-pop! (stack)
;; 	(let ((result))
;; 		(if (stack-empty-p stack)
;; 			(setf result nil)
;; 			(progn
;; 				(setf result (first (stack-this stack)))
;; 				(setf (stack-this stack) (rest (stack-this stack)))
;; 				(decf (stack-pointer stack))
;; 			)
;; 		)
;; 		
;; 		;;return
;; 		result
;; 	)
;; )