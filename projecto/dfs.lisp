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



