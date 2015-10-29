;;Grupo 43

(defconstant NUM-LINES 18)
(defconstant NUM-COLLUMNS 10)

(defconstant POSITION-FILLED 1)
(defconstant POSITION-EMPTY 0)




;;######################################################          
;;################### TIPO TABULEIRO ###################
;;######################################################
(defun cria-tabuleiro ()
	(make-array '(18 10) : element-type 'bit : initial-element 0)
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

(defun tabuleiro-preenchido-p ( tabuleiro num-linha num-coluna)
  (= (aref tabuleiro num-linha num-coluna) 1)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Returns the value of the highest filled position of 
;; 'tabuleiro'.

(defun tabuleiro-altura-coluna ( tabuleiro num-coluna)
  (let ((num-linha 0))
    (loop (if ( and (= (aref tabuleiro num-linha num-coluna) 1 ) (< num-linha 18))
               (incf num-linha)
             (return num-linha))
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Returns true if line number 'num-linha' of 'tabuleiro' is
;; completely filled.

(defun tabuleiro-linha-completa-p ( tabuleiro num-linha)
  (let ((num-coluna 0))
    (loop 	(if ( and (= (aref tabuleiro num-linha num-coluna) 1 ) (< num-coluna 10))
               (if (= num-coluna 9) (return t))
             (incf num-coluna)))
    NIL
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fills position given by (num-linha, num-coluna)
;;

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


;; (defun tabuleiro_array ( tabuleiro ) 
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
;; (defun tabuleiro-to-array ( array ) 
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
(defstruct estado pontos pecas_por_colocar pecas_colocadas tabuleiro)
(make-estado :pontos NIL :pecas_por_colocar NIL :pecas_colocadas NIL :tabuleiro NIL)
(make-estado :pontos NIL :pecas_por_colocar t :pecas_colocadas NIL :tabuleiro NIL)
(make-estado :pontos NIL :pecas_por_colocar NIL :pecas_colocadas t :tabuleiro NIL)
(make-estado :pontos NIL :pecas_por_colocar NIL :pecas_colocadas NIL :tabuleiro t)



;;#################################################          
;;################# TIPO PROBLEMA #################
;;#################################################
(defstruct problema estado_inicial solucao accoes resultado custo_caminho)
(make-problema :estado_inicial t  :solucao NIL  :accoes NIL  :resultado NIL :custo_caminho NIL)
(make-problema :estado_inicial NIL  :solucao t  :accoes NIL  :resultado NIL :custo_caminho NIL)
(make-problema :estado_inicial NIL  :solucao NIL  :accoes t  :resultado NIL :custo_caminho NIL)
(make-problema :estado_inicial NIL  :solucao NIL  :accoes NIL  :resultado t :custo_caminho NIL)
(make-problema :estado_inicial NIL  :solucao NIL  :accoes NIL  :resultado NIL :custo_caminho t)






