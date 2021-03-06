;;Grupo 43
;;David Cardoso --------- 79710
;;Henrique Caldeira ----- 75838
;;Miguel Ribeiro -------- 78437

(defconstant NUM-LINES 18)
(defconstant NUM-COLLUMNS 10)

;; Values for empty and filled position  
;; of TABULEIRO.
(defconstant POSITION-FILLED 1)
(defconstant POSITION-EMPTY 0)

;; Symbol representation of each piece in lists.
(defconstant I-SYMBOL 'I)
(defconstant L-SYMBOL 'L) 
(defconstant J-SYMBOL 'J) 
(defconstant O-SYMBOL 'O) 
(defconstant S-SYMBOL 'S)
(defconstant Z-SYMBOL 'Z) 
(defconstant T-SYMBOL 'T) 

;; Maximum amount of points possible to obtain
;; with each piece.
(defconstant I-MAX-POINTS 800)
(defconstant J-MAX-POINTS 500)
(defconstant L-MAX-POINTS 500)
(defconstant S-MAX-POINTS 300)
(defconstant Z-MAX-POINTS 300)
(defconstant T-MAX-POINTS 300)
(defconstant O-MAX-POINTS 300)

;;peca i 
(defconstant p-i0 (make-array (list 4 1) :initial-element T))
(defconstant p-i1 (make-array (list 1 4) :initial-element T))
;;peca l
(defconstant p-l0 (make-array (list 3 2) :initial-contents '((T T)(T nil)(T nil))))
(defconstant p-l1 (make-array (list 2 3) :initial-contents '((T nil nil)(T T T))))
(defconstant p-l2 (make-array (list 3 2) :initial-contents '((nil T)(nil T)(T T))))
(defconstant p-l3 (make-array (list 2 3) :initial-contents '((T T T)(nil nil T))))
;;peca j
(defconstant p-j0 (make-array (list 3 2) :initial-contents '((T T)(nil T)(nil T))))
(defconstant p-j1 (make-array (list 2 3) :initial-contents '((T T T)(T nil nil))))
(defconstant p-j2 (make-array (list 3 2) :initial-contents '((T nil)(T nil)(T T))))
(defconstant p-j3 (make-array (list 2 3) :initial-contents '((nil nil T)(T T T))))
;;peca o
(defconstant p-o0 (make-array (list 2 2) :initial-element T))
;;peca s
(defconstant p-s0 (make-array (list 2 3) :initial-contents '((T T nil)(nil T T))))
(defconstant p-s1 (make-array (list 3 2) :initial-contents '((nil T)(T T)(T nil))))
;;peca z
(defconstant p-z0 (make-array (list 2 3) :initial-contents '((nil T T)(T T nil))))
(defconstant p-z1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(nil T))))
;;peca t
(defconstant p-t0 (make-array (list 2 3) :initial-contents '((T T T)(nil T nil))))
(defconstant p-t1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(T nil))))
(defconstant p-t2 (make-array (list 2 3) :initial-contents '((nil T nil)(T T T))))
(defconstant p-t3 (make-array (list 3 2) :initial-contents '((nil T)(T T)(nil T))))

;; Lists containing all configurations of each piece.
(defconstant pecas-i (list p-i0 p-i1))
(defconstant pecas-l (list p-l0 p-l1 p-l2 p-l3))
(defconstant pecas-j (list p-j0 p-j1 p-j2 p-j3))
(defconstant pecas-o (list p-o0))
(defconstant pecas-s (list p-s0 p-s1))
(defconstant pecas-z (list p-z0 p-z1))
(defconstant pecas-t (list p-t0 p-t1 p-t2 p-t3))


;;#######################################################
;;################## AUXILIARY FUNCTIONS ################
;;############# Nao sao pedidas no enunciado ############
;;#######################################################

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARRAY-COPIA
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
;; GET-CONFIGURATIONS
;; Receives list of PECAS-POR-COLOCAR and returns a list with
;; the possible configurations of first piece.
;;TESTADO
(defun get-configurations (pecas-por-colocar)
	(let ((peca (first pecas-por-colocar)))
		(cond ((eq peca I-SYMBOL) pecas-i)
			((eq peca L-SYMBOL) pecas-l)
			((eq peca O-SYMBOL) pecas-o)
			((eq peca J-SYMBOL) pecas-j)
			((eq peca Z-SYMBOL) pecas-z)
			((eq peca S-SYMBOL) pecas-s)
			((eq peca T-SYMBOL) pecas-t)
			
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GET-SYMBOL
;; Receives a piece configuration and returns its symbol.
;;TESTADO
(defun get-symbol (peca)
	(cond ((equalp peca p-i0) I-SYMBOL)
		((equalp peca p-i1) I-SYMBOL)
		((equalp peca p-o0) O-SYMBOL)
		((equalp peca p-s0) S-SYMBOL)
		((equalp peca p-s1) S-SYMBOL)
		((equalp peca p-z0) Z-SYMBOL)
		((equalp peca p-z1) Z-SYMBOL)
		((equalp peca p-l0) L-SYMBOL)
		((equalp peca p-l1) L-SYMBOL)
		((equalp peca p-l2) L-SYMBOL)
		((equalp peca p-l3) L-SYMBOL)
		((equalp peca p-j0) J-SYMBOL)
		((equalp peca p-j1) J-SYMBOL)
		((equalp peca p-j2) J-SYMBOL)
		((equalp peca p-j3) J-SYMBOL)
		((equalp peca p-t0) T-SYMBOL)
		((equalp peca p-t1) T-SYMBOL)
		((equalp peca p-t2) T-SYMBOL)
		((equalp peca p-t3) T-SYMBOL)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PIECE-WIDTH
;; Receives a PECA and returns its width.
;; TESTADO
(defun piece-width (peca)
	(let ((result 0))
		(dotimes (line (array-dimension peca 0))
			(dotimes (collumn (array-dimension peca 1))
				(when (eq t (aref peca line collumn))
					(when (> collumn result)
						(setf result collumn)
					)
				)
			)
		)
		(1+ result)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PIECE-MAX-POINTS
;; Receives a PECA and returns the maximum amount of points 
;; possible for that PECA.
;; TESTADO
(defun piece-max-points (peca)
	(cond ((eq peca I-SYMBOL) I-MAX-POINTS)
		((eq peca L-SYMBOL) L-MAX-POINTS)
		((eq peca O-SYMBOL) O-MAX-POINTS)
		((eq peca J-SYMBOL) J-MAX-POINTS)
		((eq peca Z-SYMBOL) Z-MAX-POINTS)
		((eq peca S-SYMBOL) S-MAX-POINTS)
		((eq peca T-SYMBOL) T-MAX-POINTS)
	)
)



;;#######################################################          
;;##################### TIPO ACCAO ######################
;;#######################################################
;; Pair (COLUNA . PECA)
;; 
;; COLUNA: collumn number of leftmost position of piece.
;; 
;; PECA: 2D array with piece configuration.
;; 		ex: ((T T)(T nil)(T nil))
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CRIA-ACCAO
;; Receives a collumn number and a tetris piece configuration.
;; TESTADO
(defun cria-accao (collumn piece)
	(cons collumn piece)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACCAO-COLUNA
;; Receives an ACCAO and returns its collumn number.
;; TESTADO
(defun accao-coluna (accao)
	(car accao)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACCAO-PECA
;; Receives an ACCAO and returns its PECA.
;; TESTADO
(defun accao-peca (accao)
	(cdr accao)
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
;; Returns a copy of TABULEIRO.
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
;; of TABULEIRO is filled.
;; TESTADO
(defun tabuleiro-preenchido-p ( tabuleiro num-linha num-coluna)
  (eq (aref tabuleiro num-linha num-coluna) POSITION-FILLED)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PIECE-HEIGHT-COLLUMN
;; Returns the height of piece in a given collumn
;; 
;; example:
;; (piece-height-collumn #2A((NIL T) (NIL T) (T T)) 0)
;; 2
;;
;; TESTADO
(defun piece-height-collumn (peca collumn)
	(let ((result 0))
		(block conditions
			(loop for line from 0 to (1- NUM-LINES) do
				(when (eq t (aref peca line collumn))
					(setf result line)
					(return-from conditions)
				)
			)
		)
		
		result
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABULEIRO-COLOCA-PECA!
;; Places a tetris piece in the tabuleiro. 
;; In other words, applies an ACCAO to the TABULEIRO.
;; PARECE FUNCIONAR BEM
(defun tabuleiro-coloca-peca! (tabuleiro accao)
	(let ((coluna-inicial (accao-coluna accao)) (peca (accao-peca accao)) (tab-floor 0) )
		(block find-floor
			(dotimes (collumn (array-dimension peca 1))
				
				(when (> (tabuleiro-altura-coluna tabuleiro (+ coluna-inicial collumn)) tab-floor)
				
					
					
					(setf tab-floor (- (tabuleiro-altura-coluna tabuleiro (+ coluna-inicial collumn)) (piece-height-collumn peca collumn)))


				)
			)
		)
		
		(dotimes (line (array-dimension peca 0))
			(dotimes (collumn (array-dimension peca 1))
				(when (eq (aref peca line collumn) t)
						(tabuleiro-preenche! 
							tabuleiro 
							(+ line tab-floor)
							(+ coluna-inicial collumn)
						)
				)
			)
		)
		
		tabuleiro
	)
)

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABULEIRO-ALTURA-COLUNA
;; Returns the value of the highest filled position of 
;; TABULEIRO.
;; TESTADO
(defun tabuleiro-altura-coluna ( tabuleiro num-coluna)
	(let ((max-altura 0))
	
		(dotimes (line NUM-LINES)
			(when (eq (aref tabuleiro line num-coluna) POSITION-FILLED)
				(if (not (zerop line))
					(setf max-altura line)
					(setf max-altura 1)
				)
			)
		)
		;; Return
		(if (= 0 max-altura)
			0
			(1+ max-altura)
		)
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
;; Fills position given by (num-linha, num-coluna) of TABULEIRO.
;; TESTADO
(defun tabuleiro-preenche! (tabuleiro num-linha num-coluna)
	(if (and (>= num-linha 0) (< num-linha 18) (>= num-coluna 0) (< num-coluna 10))  
		(progn 
			(setf (aref tabuleiro num-linha num-coluna) POSITION-FILLED)
			t
		)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABULEIRO-LINHA-PREENCHIDA-P
;; Returns true if any position in given LINE is filled.
;; TESTADO
(defun tabuleiro-linha-preenchida-p (tabuleiro line) 
	(let ((result nil))
		(block loop-block
			(loop for collumn from 0 to (1- NUM-COLLUMNS)
				do
				(when (eq (aref tabuleiro line collumn) POSITION-FILLED)
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
;; Returns true if both boards are equal.
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
;; Receives a TABULEIRO and a line number, and clears that 
;; line. Each line above will drop down to the line directly
;; below it.
;; TESTADO
(defun tabuleiro-remove-linha! (tabuleiro initial-line)
	(let ((line initial-line))
		(if (tabuleiro-linha-preenchida-p tabuleiro line)
		
			(progn
				(dotimes (collumn NUM-COLLUMNS)
					(setf (aref tabuleiro initial-line collumn) 0)
				)
			
				(incf line)
				
				(loop do
					(dotimes (collumn NUM-COLLUMNS)
						(when (tabuleiro-preenchido-p tabuleiro line collumn)
							
							(setf (aref tabuleiro line collumn) POSITION-EMPTY)
							(tabuleiro-preenche! tabuleiro (1- line) collumn)
						)
					)
					(incf line)
				
					while (< line NUM-LINES )
				)
				
				t
				
			)
			
			nil
		)
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABULEIRO-ARRAY
;; Receives board as an array filled with 1's and 0's.
;; Returns board as an array with positions filled with 'true' and 'nil'.
;; TESTADO
(defun tabuleiro->array (tabuleiro) 
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
;; Receives board as an array filled with logic values.
;; Returns board as an array with positions filled with 1's and 0's.
;; TESTADO
(defun array->tabuleiro(array-log)
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
;;
;; PONTOS: number of points obtained so far.
;; 
;; PECAS-POR-COLOCAR: list of pieces not placed in 
;; their final position yet.
;;
;; PECAS-COLOCADAS: list of pieces already placed 
;; at the bottom of TABULEIRO.
;;
;; TABULEIRO: the game table, of type 'tabuleiro'.
(defstruct estado 
			(pontos 0) ;;-----------------------integer
			(pecas-por-colocar  '());;----------list
			(pecas-colocadas '());;-------------list
			(tabuleiro (cria-tabuleiro));;------board(tabuleiro)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESTADO-PONTOS-ADICIONA!
;; Increases the number of points of a given 'estado'
;; TESTADO
(defun estado-pontos-adiciona! (estado num-pontos)
	(if (not (null (estado-pontos estado)))
		(setf (estado-pontos estado) (+ (estado-pontos estado) num-pontos))
		(setf (estado-pontos estado) num-pontos)
	)
)
     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESTADO-FINAL-P
;; Returns true if ESTADO is an objective state.
;; 
;; Is an objective state if:
;; 	- Atlest one position of top line of TABULEIRO is 
;; 	filled;
;;  			OR
;; 	- List of PECAS-POR-COLOCAR is empty.
;;
;; TESTADO
(defun estado-final-p (est)
	(let ((result t))
		(block condition-block
			(if (tabuleiro-topo-preenchido-p (estado-tabuleiro est))
				(progn
					(setf result t)
					(return-from condition-block)
				)
				()
			)
			
			(when (not (null (estado-pecas-por-colocar est)))
				(setf result nil)
				(return-from condition-block)
			)
				
		)
		
		result
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESTADOS-IGUAIS-P
;; Returns true if the content of both state objects is the same.
;; TESTADO
(defun estados-iguais-p (est1 est2)
	(let ((result t))
		(block conditions-block
			(if (equal (estado-pontos est1) (estado-pontos est2))
				()
				(progn
					(setf result nil)
					(return-from conditions-block)
				)
			)
			
			(if (equalp (estado-pecas-por-colocar est1) (estado-pecas-por-colocar est2))
				()
				(progn
					(setf result nil)
					(return-from conditions-block)
				)
			)
			
			(if (equalp (estado-pecas-colocadas est1) (estado-pecas-colocadas est2))
				()
				(progn
					(setf result nil)
					(return-from conditions-block)
				)
			)
			
			(if (tabuleiros-iguais-p (estado-tabuleiro est1) (estado-tabuleiro est2))
				()
				(progn
					(setf result nil)
					(return-from conditions-block)
				)
			)
		)
		
		result
	)
)




 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COPIA-ESTADO
;; Copies a state.
;; TESTADO
(defun copia-estado (estado)
	(let ((result (make-estado)))
		
		(setf (estado-pontos result) (estado-pontos estado)) 
		(setf (estado-pecas-por-colocar result) (copy-list (estado-pecas-por-colocar estado)))
		(setf (estado-pecas-colocadas result) (copy-list (estado-pecas-colocadas estado)))
		(setf (estado-tabuleiro result) (copia-tabuleiro (estado-tabuleiro estado)))
		result
	)
)


     
     
     
     

;;#################################################          
;;################# TIPO PROBLEMA #################
;;#################################################
(defstruct problema 
			estado-inicial ;;------ type ESTADO
			solucao ;;------------- function
			accoes ;;-------------- function
			resultado ;;----------- function
			custo-caminho ;;------- function
)



;;######################################################   
;;######################## 2.2.1 #######################         
;;################# FUNCOES DO PROBLEMA ################
;;##################### DE PROCURA #####################
;;######################################################

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SOLUCAO
;; Receives a object of type ESTADO and returns true if it is a solution.
;; 
;; is_solution => not(tabuleiro-topo-preenchido-p) 
;; 			&& empty(pecas-por-colocar)
;; 
;;TESTADO
(defun solucao (estado)
	(let ((result t))
		(block condition-block
			(when (tabuleiro-topo-preenchido-p (estado-tabuleiro estado))
				(setf result nil)
				(return-from condition-block)
			)
			
			(when (not (null (estado-pecas-por-colocar estado)))
				(setf result nil)
				(return-from condition-block)
			)
		)
		
		result
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACCOES
;; Receives a ESTADO and returns list of all possible ACCAO
;; objects possible, containing the next PECA-POR-COLOCAR.
;; 
;; ESTADO -> pecas-por-colocar --->(i, o , z, t , l)
;;                                 	|
;;                                 	v
;;               (accao(0 peca-i0)), accao(1, peca-i0)), (..)
;;		     ,(accao(16, peca-i1)), (accao(17, peca-i1)))
;; 
;; TESTADO
(defun accoes (estado)
	(let ((result) (configurations))
	
			(when (estado-final-p estado)
				(return-from accoes nil)
			)
				
			(setf configurations (get-configurations (estado-pecas-por-colocar estado)))
		
			(dolist (elem configurations)
				(loop for collumn from 0 to  (1- NUM-COLLUMNS) do
					(when (>= NUM-COLLUMNS (+ collumn (piece-width elem)))
						(setf result (append result (list (cria-accao collumn elem))))
					)
				)
			)
			result
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RESULTADO
;; Receives a ESTADO and an ACCAO and returns a new ESTADO 
;; resulting of applying parameter ACCAO to parameter ESTADO. 
;; 
;; TESTADO
(defun resultado (estado accao)
	(let ((result (make-estado)) (tabuleiro (copia-tabuleiro (estado-tabuleiro estado))) (lines-removed 0) (awarded-points 0))
		(setf (estado-pecas-por-colocar result) (rest (estado-pecas-por-colocar estado)))
		
		(if (null (estado-pecas-colocadas estado))
			(setf (estado-pecas-colocadas result) (list (first (estado-pecas-por-colocar estado))))
			(setf (estado-pecas-colocadas result) (cons (first (estado-pecas-por-colocar estado)) (estado-pecas-colocadas estado)))
		)
		
		(tabuleiro-coloca-peca! tabuleiro accao) 
		
		(setf (estado-tabuleiro result)  tabuleiro)
		
		(if (tabuleiro-topo-preenchido-p (estado-tabuleiro result))
			()
			(dotimes (line NUM-LINES)
				(when (tabuleiro-linha-completa-p (estado-tabuleiro result) line)
					(incf lines-removed)
					(tabuleiro-remove-linha! (estado-tabuleiro result) line)
					(decf line)
				)
			)
		)
		
		(cond ((= lines-removed 0) (setf awarded-points 0))
			  ((= lines-removed 1) (setf awarded-points 100))
			  ((= lines-removed 2) (setf awarded-points 300))
			  ((= lines-removed 3) (setf awarded-points 500))
			  ((= lines-removed 4) (setf awarded-points 800))
			  (t (setf awarded-points 800))
		)
		
		(setf (estado-pontos result) (+ (estado-pontos estado) awarded-points))
		
		result
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUALIDADE
;; Receives a ESTADO and returns an integer corresponding to 
;; the number of points in ESTADO multiplied by -1. 
;;
(defun qualidade (estado)
	(* -1 (estado-pontos estado))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTO-OPORTUNIDADE
;; Receives an ESTADO and returns the maximum value of points 
;; possible obtained with all the PECAS-COLOCADAS.
;; ;;
;; i - 800 -> I-MAX-POINTS
;; j - 500 -> J-MAX-POINTS
;; l - 500 -> L-MAX-POINTS
;; s - 300 -> S-MAX-POINTS
;; z - 300 -> Z-MAX-POINTS
;; t - 300 -> T-MAX-POINTS
;; o - 300 -> O-MAX-POINTS
;;
;; TESTADO
(defun custo-oportunidade (estado)
	(let ((pecas (estado-pecas-colocadas estado)) (result 0))
		(dolist (elem pecas)
			(setf result (+ result (piece-max-points elem)))
		)
		
		(- result (estado-pontos estado))
	)
)

;;#################################################################################################
;;#################################################################################################
;;################################### ALGORITMOS DA SEGUNDA ENTREGA ###############################
;;#################################################################################################
;;#################################################################################################

;; Algoritmos da segunda entrega : DFS (procura-pp), A*.
;; Estruturas de dados adicionais.


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
;; - POINTER: an integer representing the number of 
;; elements on the stack.
;;
(defstruct stack 
				(this '())
				(pointer 0)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CREATE-STACK
;; Returns an empty stack.
;;
(defun create-stack ()
	(make-stack)
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


;;#####################################################         
;;##################### ALGORITHM #####################
;;#####################################################
;; Implementation of the depth first search 
;; algorithm.
;; 
;;#####################################################
;; Parameters:
;; 
;; PROBLEMA
;;
;; Estrutura com os seguintes atributos.
;; 	-ESTADO-INICIAL: o estado inicial da 
;; 	procura. E inserido na stack no inicio e a 
;; 	procura e feita a partir dai.
;;
;;  -SOLUCAO(estado): funcao teste-objectivo.
;;  E usada em todos os nos para testarem se sao 
;;  solucao do problema.
;; 
;;  -ACCOES(estado): funcao que devolve uma lista
;;  com todas as accoes possiveis de efectuar a 
;;  partir do estado actual.
;;
;;  -RESULTADO(estado, accao): funcao que aplicada 
;;  a um estado, recebe uma accao e devolve um novo
;;  estado resultante de aplicar a accao ao estado 
;;  dado. 
;;
;;  -CUSTO-CAMINHO(estado): funcao que devolve o 
;;  numero maximo de pontos possiveis de ter sido 
;;  obtidos num dado estado.
;;  E o equivalente ao custo de caminho num dado 
;;  estado.
;;
;; Para chamar uma funcao de uma variavel PROBLEMA
;; "prob":
;; (funcall (problema-solucao prob) estado1)
;;##################################################
;;
;; Devolve uma lista com todas as accoes efectuadas
;; desde o estado inicial ate ao estado objectivo.


(defun procura-pp (problema)
	
	;;################################ DECLARACOES DE VARIAVEIS ###############################################
	
	(let ((stack-estados (create-stack)) ;;-------------------------------- stack onde sao guardados os estados.
		  (estado-actual) ;;----------------------------------------------- estado a ser explorado na iteracao actual.
										                                  
		  (lista-accoes-estado-actual ) ;;---------------------------------  lista temporario para guardar todos as accoes 
										;; possiveis de efectuar sobre um estado.
										;; (retorno da funcao ACCOES(estado))
					
		  (mapa-estado-accao (make-hash-table)) ;;------------------------- hash table que mapeia um estado a accao que levou a ele
		  (mapa-estado-antecessor (make-hash-table)) ;;-------------------- hash table que mapeia um estado ao seu antecessor
	
		  (estado-resultado)			
					
		  (caminho-resultado '())) ;;-------------------------------------- guarda o caminho actual tomado (lista de accoes)
		  
		  
		  
	;;################################# FIM DE DECLARACOES ####################################################  
		 
		 
		(stack-push! stack-estados (problema-estado-inicial problema)) ;;-- poe o estado inicial na pilha
		
		(block main-loop
		
			(loop do ;;---------------------------------------------------- ciclo principal
			
				(when (stack-empty-p stack-estados) ;;--------------------- se encontrar a stack vazia, nao existe solucao.
					(return-from procura-pp nil) ;;----------------------------- sai do ciclo
				) 
				
				(setf estado-actual (stack-pop! stack-estados)) ;;--------- vai buscar o proximo estado a ser explorado a stack.
				
				
				(if (funcall (problema-solucao problema) estado-actual) ;; testa se o estado actual e solucao
					
					;;--------------------------------------------------- se o estado e solucao:
					(progn
						(return-from main-loop) ;;----------------------- Termina o algoritmo e retorna o caminho.
					)

					
					;;--------------------------------------------------- se o estado nao e solucao:
					(progn 
						;; determina a lista de accoes possiveis
						;; a partir do estado actual 
						(setf lista-accoes-estado-actual 
							(funcall (problema-accoes problema) estado-actual)
						)
						
						
						;; expande o estado actual, adicionando todos os estados possiveis a stack.
						(dolist (accao lista-accoes-estado-actual) 
							(setf estado-resultado (funcall (problema-resultado problema) estado-actual accao))
						
							(stack-push! stack-estados estado-resultado)
							
							(setf (gethash estado-resultado mapa-estado-accao) accao)
							(setf (gethash estado-resultado mapa-estado-antecessor) estado-actual)
							
						)
					)
					
				)
			
			)
			
		)
		
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STACK-ORDERED-PUSH!
;; Pushes ELEM onto the stack. Insertion order is:
;; 	- lowest value at head of stack.
;; 	- highues value at the end.
;;
;;  Values are determined by [custo(elem) + heuristica(elem)]
;; 
;; Runs in O(n) time.
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


;;#####################################################         
;;##################### ALGORITHM #####################
;;#####################################################
;; Implementation of the A* search 
;; algorithm.
;; 
;;#####################################################
;; Parameters:
;; 
;; PROBLEMA
;;
;; Estrutura com os seguintes atributos.
;; 	-ESTADO-INICIAL: o estado inicial da 
;; 	procura. E inserido na stack no inicio e a 
;; 	procura e feita a partir dai.
;;
;;  -SOLUCAO(estado): funcao teste-objectivo.
;;  E usada em todos os nos para testarem se sao 
;;  solucao do problema.
;; 
;;  -ACCOES(estado): funcao que devolve uma lista
;;  com todas as accoes possiveis de efectuar a 
;;  partir do estado actual.
;;
;;  -RESULTADO(estado, accao): funcao que aplicada 
;;  a um estado, recebe uma accao e devolve um novo
;;  estado resultante de aplicar a accao ao estado 
;;  dado. 
;;
;;  -CUSTO-CAMINHO(estado): funcao que devolve o 
;;  numero maximo de pontos possiveis de ter sido 
;;  obtidos num dado estado.
;;  E o equivalente ao custo de caminho num dado 
;;  estado.
;;
;; Para chamar uma funcao de uma variavel PROBLEMA
;; "prob":
;; (funcall (problema-solucao prob) estado1)
;;
;;	
;; HEURISTICA
;; -Funcao de calculo do valor da heuristica de
;; um estado.
;;
;;##################################################
;;
;; Devolve uma lista com todas as accoes efectuadas
;; desde o estado inicial ate ao estado objectivo.
;;
(defun procura-A* (problema heuristica)
	
	;;################################ DECLARACOES DE VARIAVEIS ###############################################
	
	(let ((stack-estados (create-stack)) ;;-------------------------------- stack onde sao guardados os estados.
		  (estado-actual) ;;----------------------------------------------- estado a ser explorado na iteracao actual.
										                                  
		  (lista-accoes-estado-actual ) ;;---------------------------------  lista temporario para guardar todos as accoes 
										;; possiveis de efectuar sobre um estado.
										;; (retorno da funcao ACCOES(estado))
					
		  (mapa-estado-accao (make-hash-table)) ;;------------------------- hash table que mapeia a um estado a accao que levou a ele
		  (mapa-estado-antecessor (make-hash-table)) ;;-------------------- hash table que mapeia um estado ao seu antecessor
	
		  (estado-resultado)			
					
		  (caminho-resultado '())) ;;-------------------------------------- guarda o caminho actual tomado (lista de accoes)
		  
		  
		  
	;;################################# FIM DE DECLARACOES ####################################################  
		 
		 
		(stack-ordered-push! stack-estados (problema-estado-inicial problema) (problema-custo-caminho problema) heuristica)
		
		(block main-loop
			
			(loop do ;;---------------------------------------------------- ciclo principal
				(when (stack-empty-p stack-estados) ;;--------------------- se encontrar a stack vazia, nao existe solucao.
					(return-from procura-A* nil) ;;------------------------ sai do ciclo
				) 
				
				
				(setf estado-actual (stack-pop! stack-estados)) ;;--------- vai buscar o proximo estado a ser explorado a stack.
				
				
				(if (funcall (problema-solucao problema) estado-actual) ;; testa se o estado actual e solucao
					
					;;--------------------------------------------------- se o estado e solucao:
					(progn
						(return-from main-loop) ;;----------------------- Termina o algoritmo e retorna o caminho.
					)
					
					
					;;--------------------------------------------------- se o estado nao e solucao:
					(progn 
					
						;; determina a lista de accoes possiveis
						;; a partir do estado actual 
						(setf lista-accoes-estado-actual 
							(funcall (problema-accoes problema) estado-actual)
						)
						
						;; expande o estado actual, adicionando todos os estados possiveis a stack.
						(dolist (accao lista-accoes-estado-actual) 
							(setf estado-resultado (funcall (problema-resultado problema) estado-actual accao))
						
							(stack-ordered-push! stack-estados estado-resultado (problema-custo-caminho problema) heuristica)
							
							(setf (gethash estado-resultado mapa-estado-accao) accao)
							(setf (gethash estado-resultado mapa-estado-antecessor) estado-actual)
							
						)
					)
					
				)
			
			)
			
		)
		
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
						  ;; nil caso nao tenho encontrado solucao. 
	)
)


;;#####################################################         
;;##################### HEURISTIC #####################
;;##################### FUNCTIONS #####################
;;#####################################################

;; HEURISTICA0: heuristica fraca e experimental. 
;; Retorna o numero de pecas por colocar.
(defun heuristica0 (estado)
	(length (estado-pecas-por-colocar estado))
)

;; HEURISTICA-H1: numero de 'buracos' (espacos 
;; abertos por baixo de espacos preenchidos).
(defun heuristica-h1 (estado)
	(let (
			(tabuleiro (estado-tabuleiro estado))
			(height-curr-collumn 0)
			(num-holes 0)
		 )
		 
		(dotimes (collumn NUM-COLLUMNS)
			
			(setf height-curr-collumn (tabuleiro-altura-coluna tabuleiro collumn))
			
			
			(loop for line from 0 to (- height-curr-collumn 2) do
				(when (= (aref tabuleiro line collumn) 0)
					(incf num-holes)
				)
			)
		)

	num-holes
	)
)
 

;; HEURISTICA-H2: numero de blocos da coluna com 
;; maior numero de blocos.
(defun heuristica-h2 (estado)
	(let ((max-num-blocks 0)
		  (num-blocks-curr-collumn 0)
		  (tabuleiro (estado-tabuleiro estado))
		 )
	
		(dotimes (collumn NUM-COLLUMNS)
			
			(setf num-blocks-curr-collumn 0)
			
			(dotimes (line NUM-LINES)
				(when (= (aref tabuleiro line collumn) POSITION-FILLED)
					  (incf num-blocks-curr-collumn)
				)
			)
			
			(when (> num-blocks-curr-collumn max-num-blocks)
				  (setf max-num-blocks num-blocks-curr-collumn)
			)
			
		)
		
		max-num-blocks
	
	)
)	

;; HEURISTICA-H3: media das alturas das colunas.
(defun heuristica-h3 (estado)
	(let (
		 (soma-alturas 0)
		 )
		 
		 (dotimes (collumn NUM-COLLUMNS)
			
			(setf soma-alturas (+ soma-alturas (tabuleiro-altura-coluna (estado-tabuleiro estado) collumn)))
		 )
		 
		 (/ soma-alturas NUM-COLLUMNS)
	)
)

;; HEURISTICA-H7: soma pesada das alturas das colunas.
;; testado, de maneira a que as colunas centrais sao 
;; mais pesadas.
(defun heuristica-h7 (estado)
	(let (
		  (tabuleiro (estado-tabuleiro estado))
		  (lista-alturas '())
		  (pesos '(0 2 4 8 16 16 8 4 2 0))
		  (resultado 0)
		 )
		 
		 (dotimes (collumn NUM-COLLUMNS)
			(setf lista-alturas (cons (tabuleiro-altura-coluna tabuleiro collumn) lista-alturas))
		 )
		
		
		(dolist (elem lista-alturas)
			(setf resultado 
				(+ resultado (* elem (first pesos)))
			)
			(setf pesos (rest pesos))
		)
		
		resultado
		
		
	)
)

;;#####################################################         
;;#################### PROCURA BEST ###################
;;#####################################################
;; 
;;#####################################################
;; Parameters:
;; 
;; TABULEIRO
;; -tabuleiro inicial do jogo.
;; 
;; PECAS-POR-COLOCAR
;; -pecas inicialmente por colocar
;;
;;##################################################
;;
;; Devolve uma lista com todas as accoes efectuadas
;; desde o estado inicial ate ao estado objectivo.
;;

(defun procura-best (tabuleiro pecas-por-colocar)
	(let ((estado-inicial (make-estado :tabuleiro (array->tabuleiro tabuleiro) :pecas-por-colocar pecas-por-colocar))
		  (problema)
		)
		
		(setf problema (make-problema :estado-inicial estado-inicial :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'qualidade))
	
		(procura-A* problema #'heuristica-h1)
	)
)





;; ###########################################
;; (load (compile-file "utils.lisp"))
(load "utils.fas") 
;  ###########################################
