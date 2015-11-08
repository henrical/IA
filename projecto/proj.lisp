;;Grupo 43

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
;; 
;;
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
;; COLUNA: collumn number of leftmost(mais a esquerda)
;; position of piece.
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
	(first accao)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACCAO-PECA
;; Receives an ACCAO and returns its PECA.
;; TESTADO
(defun accao-peca (accao)
	(rest accao)
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
(defun tabuleiro-remove-linha! (#|tab line|#)
	;;TODO
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
;; their final position yet, known when ESTADO is 
;; created.
;;
;; PECAS-COLOCADAS: list of pieces already placed 
;; at the bottom of TABULEIRO.
;;
;; TABULEIRO: the game table, of type 'tabuleiro'.
(defstruct estado 
			pontos ;;----------------------inteiro
			pecas-por-colocar ;;-----------lista
			pecas-colocadas ;;-------------lista
			tabuleiro ;;-------------------tipo tabuleiro
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
;; ESTADO-PECA-ADICIONA!
;; Adds a piece to the 'pecas-por-colocar' list.
;; TESTADO
(defun estado-peca-adiciona! (estado peca)
	(append (estado-pecas-por-colocar estado) (list peca))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESTADO-PECA-COLOCADA-ADICIONA!
;; Adds a piece to the 'pecas-colocadas' list.
;; TESTADO
(defun estado-peca-colocada-adiciona! (estado peca)
	(append (estado-pecas-colocadas estado) (list peca))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESTADO-FINAL-P
;; Returns true if ESTADO is an objective state:
;; 	- Atlest one position of top line of TABULEIRO is 
;; 	filled;
;; 	- List of PECAS-POR-COLOCAR is empty.
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
;; Returns true if both ESTADO objects are equal.
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
;; Copies a ESTADO.
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
			estado-inicial 
			solucao 
			accoes 
			resultado 
			custo-caminho
)



;;######################################################   
;;######################## 2.2.1 #######################         
;;################# FUNCOES DO PROBLEMA ################
;;##################### DE PROCURA #####################
;;######################################################

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SOLUCAO
;; Receives a ESTADO and returns true if it is a solution.
;; 
;; is_solution => not(tabuleiro-topo-preenchido-p) 
;; 			&& empty(pecas-por-colocar)
;; 
;; SEEMS TO BE WORKING
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
		(setf configurations (get-configurations (estado-pecas-por-colocar estado)))
		
		(dolist (elem configurations)
			(loop for collumn from 0 to  (1- NUM-COLLUMNS) do
				(when (>= NUM-COLLUMNS (+ collumn (piece-width elem)))
					(setf result (append result (list (cria-accao collumn elem))))
;; 					(print (cria-accao collumn elem))
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
;; 
;; (defun resultado (estado accao)
;; 	;;TODO
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUALIDADE
;; Receives a ESTADO and returns an integer corresponding to 
;; the number of points in ESTADO multiplied by -1. 
;; 
;;
(defun qualidade (estado)
	(* -1 (estado-pontos estado))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTO-OPORTUNIDADE
;; Receives an ESTADO and returns the maximum value of points 
;; possible obtained with all the PECAS-COLOCADAS.
;; 
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
		
		result
	)
)



;; ###########################################
;; (load (compile-file "utils.lisp"))
(load "utils.fas") 
;  ###########################################
