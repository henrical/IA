;;Grupo 43

(defconstant NUM-LINES 18)
(defconstant NUM-COLLUMNS 10)

(defconstant POSITION-FILLED 1)
(defconstant POSITION-EMPTY 0)

(defconstant MAX-PECAS-POR-COLOCAR 50)
(defconstant MAX-PECAS-COLOCADAS 50)

;;#######################################################
;;################## AUXILIARY FUNCTIONS ################
;;############# Nao sao pedidas no enunciado ############
;;#######################################################

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARRAY-COPY
;; Returns copy of given 2D array.
;; TESTADO
(defun array-copia (old-array)
	(let ((line-num (array-dimension old-array 0))
		(collumn-num (array-dimension old-array 1)))
		
		(let ((new-array (make-array (list line-num collumn-num))))
		
			(dotimes (line line-num)
				(dotimes (collumn collumn-num)
					(setf (aref new-array line collumn) (aref old-array line collumn))
				)
			)
		
			new-array
		)
	)
)



;;#######################################################          
;;##################### TIPO ACCAO ######################
;;#######################################################
;; COLUNA: collumn number of leftmost(mais a esquerda)
;; position of piece.
;; 
;; PECA: 2D array with piece configuration.
;; 		ex: ((T T)(T nil)(T nil))
;;
;; Usage:
;; (setf a1 (cria-accao :coluna 0 :peca peca-i0))
;; (accao-coluna a1) -> 0
;; (accao-peca a1) -> copia de peca-i0
(defstruct accao 	
			coluna 
			peca
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CRIA-ACCAO
;; Receives a collumn number and a tetris piece configuration.
;; Returns a new ACCAO struct.
;; TESTADO
(defun cria-accao (collumn piece)
	(let ((result (make-accao :coluna collumn :peca (array-copia piece))))
		result
	)
)







;;######################################################          
;;################### TIPO TABULEIRO ###################
;;######################################################
;; 2D array with dimensions (NUM-LINES, NUM-COLLUMNS).
;; Filled positions contain POSITION-FILLED.
;; Empty positions contain POSITION-EMPTY.
(defun cria-tabuleiro ()
	(make-array (list NUM-LINES NUM-COLLUMNS) :element-type 'bit :initial-element POSITION-EMPTY)
)
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COPIA-TABULEIRO
;; Returns a copy of 'tabuleiro'
;; TESTADO
(defun copia-tabuleiro (tabuleiro)
  (let ((tabuleiro-novo (make-array (list NUM-LINES NUM-COLLUMNS) :initial-element POSITION-EMPTY) ))
	(dotimes (line NUM-LINES)
		(dotimes (collumn NUM-COLLUMNS)
			(when (tabuleiro-preenchido-p tabuleiro line collumn)
				(tabuleiro-preenche! tabuleiro-novo line collumn)
			)
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
		(dotimes (line NUM-LINES)
			(when (eq (aref tabuleiro line num-coluna) POSITION-FILLED)
				(setf max-altura line)
			)
		)
		;; Return:
		(1+ max-altura)
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
		(dotimes (collumn NUM-COLLUMNS)
			(when (eq (aref tabuleiro num-linha collumn) POSITION-EMPTY)
				(setf result nil)
			)
		)	
	;; Return:
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
		() 
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
		;; Return:
		result
	)
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABULEIRO-ARRAY
;; Receives an array filled with 1's and 0's.
;; Returns an array with positions filled with 'true' and 'nil'
;; TESTADO
(defun tabuleiro-array (tabuleiro) 
	(let ((result (make-array (list NUM-LINES NUM-COLLUMNS) :initial-element nil)))
		(dotimes (line NUM-LINES)
			(dotimes (collumn NUM-COLLUMNS)
				(when (tabuleiro-preenchido-p tabuleiro line collumn)
					(setf (aref result line collumn) t)
				)
			)
		)
	result
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABULEIRO-ARRAY
;; Receives an array filled with logic values.
;; Returns an array with positions filled with 1's and 0's.
;; TESTADO
(defun array-tabuleiro(array-log)
	(let ((result (make-array (list NUM-LINES NUM-COLLUMNS) :initial-element 0)))
		(dotimes (line NUM-LINES)
			(dotimes (collumn NUM-COLLUMNS)
				(when (eq (aref array-log line collumn) t)
					(tabuleiro-preenche! result line collumn)
				)
			)
		)
	result
	)
)






;;###################################################          
;;################### TIPO ESTADO ###################
;;###################################################
;; Type ESTADO, describes the state of one game 
;; (one 'tabuleiro')
;;
;; PONTOS: number of points obtained so far.
;; 
;; PECAS-POR-COLOCAR: list of pieces not placed in 
;; their final position yet (i.e. they are still
;; floating?).
;;
;; PECAS-COLOCADAS: list of pienes already placed 
;; at the bottom of TABULEIRO.
;;
;; TABULEIRO: the game table, of type 'tabuleiro'.
(defstruct estado 
			pontos 
			pecas-por-colocar
			pecas-colocadas 
			tabuleiro
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COPIA-ESTADO
;; Creates a new empty 'estado'
;; TESTADO
(defun cria-estado ()
	(let ((result (make-estado 
					:pontos 0 
					:pecas-por-colocar (make-array (list MAX-PECAS-POR-COLOCAR)) 
					:pecas-colocadas (make-array (list MAX-PECAS-COLOCADAS)) 
					:tabuleiro (cria-tabuleiro))
		))
		result
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESTADO-INC-PONTOS
;; Increases the number of points of a given 'estado'
;; TESTADO
(defun estado-inc-pontos (estado num-pontos)
	(setf (estado-pontos estado) (+ (estado-pontos estado) num-pontos))
)
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESTADO-ADICIONA-PECA
;; Adds a 'peca-por-colocar' to the list.
;; TESTADO
(defun estado-adiciona-peca (estado peca)
	
)
     
     
     
     

;;#################################################          
;;################# TIPO PROBLEMA #################
;;#################################################
(defstruct problema estado-inicial solucao accoes resultado custo-caminho)

(make-problema :estado-inicial t  :solucao NIL  :accoes NIL  :resultado NIL :custo-caminho NIL)
(make-problema :estado-inicial NIL  :solucao t  :accoes NIL  :resultado NIL :custo-caminho NIL)
(make-problema :estado-inicial NIL  :solucao NIL  :accoes t  :resultado NIL :custo-caminho NIL)
(make-problema :estado-inicial NIL  :solucao NIL  :accoes NIL  :resultado t :custo-caminho NIL)
(make-problema :estado-inicial NIL  :solucao NIL  :accoes NIL  :resultado NIL :custo-caminho t)












;; ###########################################
(load (compile-file "utils.lisp"))
;;(load "utils.fas") 
;  ###########################################
