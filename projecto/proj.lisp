;;Grupo 43

(defconstant NUM-LINES 18)
(defconstant NUM-COLLUMNS 10)

(defconstant POSITION-FILLED 1)
(defconstant POSITION-EMPTY 0)

(defconstant MAX-PECAS 50)


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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARRAY-PECAS-COPIA
;; Returns copy of given 1D array of 'pecas'.
;; Exp: (peca-i0, peca-i1, NIL, NIL) -> (peca-i0, peca-i1, NIL, NIL)
;; TESTADO
(defun array-pecas-copia (old-array)
	(let ((result (make-array (list MAX-PECAS))))
		(dotimes (index MAX-PECAS)
			(when (not (null (aref old-array index)))
				(setf (aref result index) (aref old-array index))
			)
		)
		result
	)
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PECAS-IGUAIS-P
;; Returns true if both tetris pieces are equal.
;; POR TESTAR!!!!!!!!
(defun pecas-iguais-p (p1 p2)
	(let ((p1-line-num (array-dimension p1 0))
		 (p2-line-num (array-dimension p2 0))
		 (p1-collumn-num (array-dimension p1 1))
		 (p2-collumn-num (array-dimension p2 1))
		 (result t)
	     )
	     
	     (cond ((not (= p1-line-num p2-line-num)) (setf result nil))
		     ((not (= p1-collumn-num p2-collumn-num)) (setf result nil))
		     (t 
				(loop for line from 0 to (1- p1-line-num) do
					(loop for collumn from 0 to (1- p1-collumn-num) do
						(when (not (eq (aref p1 line collumn) (aref p2 line collumn)))
							(setf result nil)
							(return)
						)
						
					)
				)
			)
	     )
	     
	     result
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARRAY-PECAS-IGUAIS-P
;; Returns true if both arrays (holding tetris pieces) are 
;; equal.
;; POR TESTAR!!!!!!!!
(defun array-pecas-iguais-p (arr1 arr2)
	(let ((result t))
		(loop for index from 0 to (1- (array-dimension arr1 0)) do
			(when (not (pecas-iguais-p (aref arr1 index) (aref arr2 index))) 
				(setf result nil)
				(return)
			)
		)	
		result
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
;; TABULEIRO-REMOVE-LINHA!
;; Receives a 'tabuleiro' and a line number, and clears that 
;; line. All lines above will drop down until they find a 
;; 'floor'.
;; (defun tabuleiro-remove-linha! (tab line)
;; 	;;TODO
;; )



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
;; CRIA-ESTADO
;; Creates a new empty 'estado'
;; TESTADO
(defun cria-estado ()
	(let ((result (make-estado 
					:pontos 0 
					:pecas-por-colocar (make-array (list MAX-PECAS)) 
					:pecas-colocadas (make-array (list MAX-PECAS)) 
					:tabuleiro (cria-tabuleiro))
		))
		result
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESTADO-PONTOS-ADICIONA!
;; Increases the number of points of a given 'estado'
;; TESTADO
(defun estado-pontos-adiciona! (estado num-pontos)
	(setf (estado-pontos estado) (+ (estado-pontos estado) num-pontos))
)
     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESTADO-PECA-ADICIONA!
;; Adds a piece to the 'pecas-por-colocar' list.
;; TESTADO
(defun estado-peca-adiciona! (estado peca)
	(loop for index from 0 to MAX-PECAS
		do
		(when (null (aref (estado-pecas-por-colocar estado) index))
			(setf (aref (estado-pecas-por-colocar estado) index) peca)
			(return)
		)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESTADO-PECA-COLOCADA-ADICIONA!
;; Adds a piece to the 'pecas-colocadas' list.
;; TESTADO
(defun estado-peca-colocada-adiciona! (estado peca)
	(loop for index from 0 to MAX-PECAS
		do
		(when (null (aref (estado-pecas-colocadas estado) index))
			(setf (aref (estado-pecas-colocadas estado) index) peca)
			(return)
		)
	)
)
 
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COPIA-ESTADO
;; Adds a piece to the 'pecas-colocadas' list.
;; TESTADO
(defun copia-estado (estado)
	(let ((result (make-estado 
					:pontos 0 
					:pecas-por-colocar (make-array (list MAX-PECAS)) 
					:pecas-colocadas (make-array (list MAX-PECAS)) 
					:tabuleiro (cria-tabuleiro))
		))
		
		(setf (estado-pontos result) (estado-pontos estado)) 
		(setf (estado-pecas-por-colocar result) (array-pecas-copia (estado-pecas-por-colocar estado)))
		(setf (estado-pecas-colocadas result) (array-pecas-copia (estado-pecas-colocadas estado)))
		(setf (estado-tabuleiro result) (copia-tabuleiro (estado-tabuleiro estado)))
		result
	)
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
