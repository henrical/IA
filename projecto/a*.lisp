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
		  (curr-elem-val (+ (funcall custo curr-elem) (funcall heuristica curr-elem)))
		  (stack-pre-list '())
		  (stack-post-list (stack-this stack))
		  (curr-elem (first stack-post-list))
		  (result '())
		 )
	
		(if (stack-empty-p stack)
			(progn
				(setf (stack-this stack) (list elem))
				(incf (stack-pointer stack))
			)
			(progn
		
				(block insertion-loop
					(loop do
					
						(if (<= new-elem-value curr-elem-value)
							(progn
								(setf result (append (reverse stack-pre-list) (cons elem stack-post-list))) 
							)
							(progn
								(setf stack-pre-list (cons curr-elem stack-pre-list))
								(setf stack-post-list (rest stack-post-list))
								
								(setf curr-elem (first stack-post-list))
								(setf curr-elem-val (+ (funcall custo curr-elem) (funcall heuristica curr-elem)))
							)
						)
					
						
					)
				)
			)
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