;;#####################################################         
;;##################### HEURISTIC #####################
;;##################### FUNCTIONS #####################
;;#####################################################
;;
(defun heuristica1 (estado)
	(length (estado-pecas-por-colocar estado))
)

;;#####################################################         
;;##################### ALGORITHM #####################
;;#####################################################
;; Implementation of the recursive best first search 
;; algorithm.
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
	(let ((estado-inicial (make-estado :pontos 0 :pecas-por-colocar pecas-por-colocar :pecas-colocadas '() :tabuleiro tabuleiro))
		)
	
		estado-inicial
	)
)