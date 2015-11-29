;;teste tempo

(ignore-value (setf t1 (cria-tabuleiro)))

(ignore-value (dotimes (linha 16) (dotimes (coluna 7) (tabuleiro-preenche! t1 linha coluna))))


(procura-best t1 '(l j i z o))
;;###################################################

(ignore-value (setf t1 (cria-tabuleiro)))

(ignore-value (dotimes (linha 16) (dotimes (coluna 7) (tabuleiro-preenche! t1 linha coluna))))

(procura-best t1 '(l j))