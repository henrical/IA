;;#####################################################         
;;##################### HEURISTIC #####################
;;##################### FUNCTIONS #####################
;;#####################################################
;;
(defun heuristica1 (estado)
	(length (estado-pecas-por-colocar estado))
)

(defun heuristica-h6 (estado)
	()
)

(defun heuristica-h7 (estado)

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
(defvar pecas-por-colocar '(i i i))
(defvar tabuleiro (cria-tabuleiro))
(defvar estado-init (make-estado :tabuleiro tabuleiro :pecas-por-colocar pecas-por-colocar))
;; (executa-jogadas estado-init (procura-best tabuleiro pecas-por-colocar))



(defun procura-best (tabuleiro pecas-por-colocar)
	(let ((estado-inicial (make-estado :tabuleiro tabuleiro :pecas-por-colocar pecas-por-colocar))
;; 		  (heuristica #'(lambda (x) 0))
		  (problema)
		)
		
		(setf problema (make-problema :estado-inicial estado-inicial :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'custo-oportunidade))
	
		(procura-A* problema #'heuristica1)
	)
)

