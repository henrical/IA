(defvar est1 (cria-estado))
(estado-peca-adiciona! est1 peca-i0)
(estado-pontos-adiciona! est1 150)
(defvar est2 (copia-estado est1))

(defvar e2 (make-estado :tabuleiro t1 :pecas-por-colocar '()))

(setf t1 (cria-tabuleiro))
(dotimes (coluna 9)
	(tabuleiro-preenche! t1 0 coluna))
(dotimes (coluna 9)
	(tabuleiro-preenche! t1 1 coluna))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; PECAS-IGUAIS-P
;; ;; Returns true if both tetris pieces are equal.
;; ;; POR TESTAR!!!!!!!!
;; (defun pecas-iguais-p (p1 p2)
;; 	(let ((p1-line-num (array-dimension p1 0))
;; 		 (p2-line-num (array-dimension p2 0))
;; 		 (p1-collumn-num (array-dimension p1 1))
;; 		 (p2-collumn-num (array-dimension p2 1))
;; 		 (result t)
;; 	     )
;; 	     
;; 	     (cond ((not (= p1-line-num p2-line-num)) (setf result nil))
;; 		     ((not (= p1-collumn-num p2-collumn-num)) (setf result nil))
;; 		     (t 
;; 				(loop for line from 0 to (1- p1-line-num) do
;; 					(loop for collumn from 0 to (1- p1-collumn-num) do
;; 						(when (not (eq (aref p1 line collumn) (aref p2 line collumn)))
;; 							(setf result nil)
;; 							(return)
;; 						)
;; 						
;; 					)
;; 				)
;; 			)
;; 	     )
;; 	     
;; 	     result
;; 	)
;; )
;; 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ARRAY-PECAS-IGUAIS-P
;; ;; Returns true if both arrays (holding tetris pieces) are 
;; ;; equal.
;; ;; POR TESTAR!!!!!!!!
;; (defun array-pecas-iguais-p (arr1 arr2)
;; 	(let ((result t))
;; 		(loop for index from 0 to (1- (array-dimension arr1 0)) do
;; 			(when (not (pecas-iguais-p (aref arr1 index) (aref arr2 index))) 
;; 				(setf result nil)
;; 				(return)
;; 			)
;; 		)	
;; 		result
;; 	)
;; )



;; (array-pecas-iguais-p (estado-pecas-por-colocar est1) (estado-pecas-por-colocar est2))