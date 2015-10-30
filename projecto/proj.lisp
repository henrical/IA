;;Grupo 43

(defconstant NUM-LINES 18)
(defconstant NUM-COLLUMNS 10)

;; So para o desenvolvimento, depois muda-se para 1 e 0.
(defconstant POSITION-FILLED 1)
(defconstant POSITION-EMPTY 0)


;;######################################################          
;;################### TIPO TABULEIRO ###################
;;######################################################
(defun cria-tabuleiro ()
	(make-array (list NUM-LINES NUM-COLLUMNS) #|: element-type 'bit|# : initial-element POSITION-EMPTY)
)
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COPIA-TABULEIRO
;; Returns a copy of 'tabuleiro'
;; TESTADO

(defun copia-tabuleiro (tabuleiro)
  (let ((tabuleiro-novo (make-array (list NUM-LINES NUM-COLLUMNS) : element-type 'bit)))
	(dotimes (line (1- NUM-LINES))
		(dotimes (collumn (1- NUM-COLLUMNS))
			(setf (aref tabuleiro-novo line collumn) (aref tabuleiro line collumn))
		)
	)
	tabuleiro-novo
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABULEIRO-PREENCHIDO-P
;; Returns true if position (num-linha, num-coluna) 
;; of 'tabuleiro' is filled.
;; TESTADO

(defun tabuleiro-preenchido-p ( tabuleiro num-linha num-coluna)
  (eq (aref tabuleiro num-linha num-coluna) POSITION-FILLED)
)

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABULEIRO-ALTURA-COLUNA
;; Returns the value of the highest filled position of 
;; 'tabuleiro'.
;; TESTADO

(defun tabuleiro-altura-coluna ( tabuleiro num-coluna)
	(let ((max-altura 0))
		(dotimes (line (1- NUM-LINES))
			(when (eq (aref tabuleiro line num-coluna) POSITION-FILLED)
				(setf max-altura line)
			)
		)
		;;return
		max-altura 
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABULEIRO-LINHA-COMPLETA-P
;; Returns true if line number 'num-linha' of 'tabuleiro' is
;; completely filled.
;; TESTADO

(defun tabuleiro-linha-completa-p ( tabuleiro num-linha)
	(let ((result t))
		(dotimes (collumn (1- NUM-COLLUMNS))
			(when (eq (aref tabuleiro num-linha collumn) POSITION-EMPTY)
				(setf result nil)
			)
		)	
	;;return
	result
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABULEIRO-PREENCHE!
;; Fills position given by (num-linha, num-coluna)
;; TESTADO

(defun tabuleiro-preenche! (tabuleiro num-linha num-coluna)
	(if (and (>= num-linha 0) (< num-linha 18) (>= num-coluna 0) (< num-coluna 10))  
		(setf (aref tabuleiro num-linha num-coluna) POSITION-FILLED)
		() ;;returns NIL
	)
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABULEIRO-TOPO-PREENCHIDO-P
;; Returns true if any position in collumn number 17 is filled.
;; TESTADO

(defun tabuleiro-topo-preenchido-p (tabuleiro) 
	(let ((result nil) (last-line (1- NUM-LINES)))
		(block loop-block
			(loop for collumn from 0 to (1- NUM-COLLUMNS)
				do
				(when (eq (aref tabuleiro last-line collumn) POSITION-FILLED)
					(setf result t)
					(return-from loop-block)
				)
			)
		)
	result
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABULEIROS-IGUAIS-P
;; Returns true if both 'tabuleiro' parameters are equal.
;; TESTADO

(defun tabuleiros-iguais-p (tab1 tab2)
	(let ((result t))
		(block loop-block
			;; Loop through all positions in 2D array;
			(loop for x from 0 to (1- NUM-LINES)
				do 
				(loop for y from 0 to (1- NUM-COLLUMNS)
					do
					;; If position (x,y) is equal in both tables;
					(if (eq (aref tab1 x y) (aref tab2 x y))
						;; Do nothing;
						()
						;; Else, execute several instructions;
						(progn 
							;; Set 'result' to false;
							(setf result nil)
							;; Exit loop;
							(return-from loop-block)
						)
					)
				)
			)
		)
		;;Return:
		result
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (defun tabuleiro-array ( tabuleiro ) 
;;   (setf tabuleiro_logico (make-array '(18 10))
;;     (let (num_linha 0))
;;     (let (num_coluna 0))
;;     (loop 	(if  (< num_coluna 10))
;;           (if (= aref tabuleiro num_linha num_coluna 1) 
;;               (setf (aref tabuleiro_logico num_linha num_coluna) true ) )
;;           (setf (aref tabuleiro_logico num_linha num_coluna) NIL )
;;           (if (= num_coluna 9)
;;               (if (= num_linha 17) (return tabuleiro_logico))
;;             (let (num_coluna 0))
;;             (incf num_linha))
;;           (incf num_coluna)))
;;   (return NIL))

;;   
;;   
;; (defun array-tabuleiro( array ) 
;;   (setf tabuleiro_binario (make-array '(18 10))
;;     (let (num_linha 0))~
;;     (let (num_coluna 0))
;;     (loop 	(if  (< num_coluna 10))
;;           (if (= aref array num_linha num_coluna T) 
;;               (setf (aref tabuleiro_binario num_linha num_coluna) 1 ) )
;;           (setf (aref tabuleiro_binario num_linha num_coluna) 0 )
;;           (if (= num_coluna 9)
;;               (if (= num_linha 17)
;;                   (return tabuleiro_binario))
;;             (let (num_coluna 0))
;;             (incf num_linha))
;;           (incf num_coluna))))

          
          
;;###################################################          
;;################### TIPO ESTADO ###################
;;###################################################
(defstruct estado pontos pecas-por-colocar pecas-colocadas tabuleiro)

(make-estado :pontos NIL :pecas-por-colocar NIL :pecas-colocadas NIL :tabuleiro NIL)
(make-estado :pontos NIL :pecas-por-colocar t :pecas-colocadas NIL :tabuleiro NIL)
(make-estado :pontos NIL :pecas-por-colocar NIL :pecas-colocadas t :tabuleiro NIL)
(make-estado :pontos NIL :pecas-por-colocar NIL :pecas-colocadas NIL :tabuleiro t)



;;#################################################          
;;################# TIPO PROBLEMA #################
;;#################################################
(defstruct problema estado-inicial solucao accoes resultado custo-caminho)

(make-problema :estado-inicial t  :solucao NIL  :accoes NIL  :resultado NIL :custo-caminho NIL)
(make-problema :estado-inicial NIL  :solucao t  :accoes NIL  :resultado NIL :custo-caminho NIL)
(make-problema :estado-inicial NIL  :solucao NIL  :accoes t  :resultado NIL :custo-caminho NIL)
(make-problema :estado-inicial NIL  :solucao NIL  :accoes NIL  :resultado t :custo-caminho NIL)
(make-problema :estado-inicial NIL  :solucao NIL  :accoes NIL  :resultado NIL :custo-caminho t)



;;#################################################          
;;################### TIPO ACCAO ##################
;;#################################################
;; COLUNA: collumn number of leftmost piece.
;; 
;; PECA: 2D array with piece configuration.
;; 		ex: ((T T)(T nil)(T nil))

(defstruct accao coluna peca)

(make-accao :coluna 0  :peca (make-array '(3 2) :initial-contents '((T T)(T nil)(T nil))))
(make-accao :coluna 0  :peca (make-array (list 2 3) :initial-contents '((T nil nil)(T T T))))


