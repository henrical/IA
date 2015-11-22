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