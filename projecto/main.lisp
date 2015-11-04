(defvar est1 (cria-estado))
(estado-peca-adiciona! est1 peca-i0)
(estado-pontos-adiciona! est1 150)
(defvar est2 (copia-estado est1))

;; (array-pecas-iguais-p (estado-pecas-por-colocar est1) (estado-pecas-por-colocar est2))