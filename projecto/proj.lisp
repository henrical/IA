;;Grupo 43

(defconstant NUM-LINES 18)
(defconstant NUM-COLLUMNS 10)

(defconstant POSITION-FILLED 1)
(defconstant POSITION-EMPTY 0)




;;######################################################          
;;################### TIPO TABULEIRO ###################
;;######################################################
(defun cria-tabuleiro ()
	(make-array '(NUM-LINES NUM-COLLUMNS) : element-type 'bit : initial-element 0)
)
  
  
;; (defun copia_tabuleiro (tabuleiro)
;;   (setf tabuleiro_novo (make-array '(18 10) : element-type 'bit))
;;     (let (num_linha 0))
;;     (let (num_coluna 0))
;;     (loop 	(if  (< num_coluna 10))
;;           (setf (aref tabuleiro_novo num_linha num_coluna) (aref tabuleiro num_linha num_coluna))
;;           (if (= num_coluna 9)
;;               (if (= num_linha 17) (return tabuleiro_novo))
;;             (let (num_coluna 0))
;;             (incf num_linha))
;;           (incf num_coluna)))
;;   (return NIL))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Returns true if position (num-linha, num-coluna) 
;; of 'tabuleiro' is filled.
;; TESTADO

(defun tabuleiro-preenchido-p ( tabuleiro num-linha num-coluna)
  (= (aref tabuleiro num-linha num-coluna) 1)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Returns the value of the highest filled position of 
;; 'tabuleiro'.
;; TESTADO

(defun tabuleiro-altura-coluna ( tabuleiro num-coluna)
  (let ((max-altura 0))
	(dotimes (line (1- NUM-LINES))
		(when (= (aref tabuleiro line num-coluna) 1)
			(setf max-altura line)
		)
	)
	#|return|#
	max-altura 
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Returns true if line number 'num-linha' of 'tabuleiro' is
;; completely filled.
;; TESTADO

(defun tabuleiro-linha-completa-p ( tabuleiro num-linha)
	(let ((result t))
		(dotimes (collumn (1- NUM-COLLUMNS))
			(when (= (aref tabuleiro num-linha collumn) 0)
				(setf result nil)
			)
		)	
	#|return|#
	result
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; Returns true if any position in collumn number 17 is filled.
;;

(defun tabuleiro-topo-preenchido-p ( tabuleiro) 
  (let ((num-coluna 0))
    (let ((num-linha 17))
      (loop (if (and (= (aref tabuleiro num-linha num-coluna) 0) (< num-coluna 10))
            (if (= num-coluna 9) (return NIL))
		(incf num-coluna))
	)
      t ;;retorna true
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; needs to be re-written
;;
;; (defun tabuleiros_iguais_p ( tabuleiro1 tabuleiro2) 
;;   (let ((num_linha 0))
;;   (let ((num_coluna 0))
;;   (loop (if  (= (aref tabuleiro1 num_linha num_coluna) (aref tabuleiro2 num_linha num_coluna))
;;             (incf num_coluna)
;;           (if (= num_coluna 10)
;;               (if (= num_linha 17) (return true))
;;             (let (num_coluna 0))
;;             (incf num_linha))))
;;   (return NIL)
;; )


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


