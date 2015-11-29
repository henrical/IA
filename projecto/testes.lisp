;; Testes da segunda entrega.

;;; Teste 13 E2
;;; procura profundidade primeiro num tabuleiro vazio, e num tabuleiro onde nao existe solucao
;;deve retornar IGNORE
(ignore-value (setf t1 (cria-tabuleiro)))
;;deve retornar uma lista de accoes (ver ficheiro output)
(procura-pp (make-problema :estado-inicial (make-estado :pontos 0 :tabuleiro t1 :pecas-colocadas () :pecas-por-colocar '(j l t o z s i)) :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'(lambda (x) 0)))
;;deve retornar IGNORE
(tabuleiro-preenche! t1 17 0)
;;deve retornar NIL (nao existe solucao)
(procura-pp (make-problema :estado-inicial (make-estado :pontos 0 :tabuleiro t1 :pecas-colocadas () :pecas-por-colocar '(j l t o z s i)) :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'(lambda (x) 0)))

((0 . #2A((T T) (T T))) (7 . #2A((T T T) (NIL NIL T)))
 (0 . #2A((NIL T) (T T) (NIL T))) (5 . #2A((NIL T) (T T) (T NIL)))
 (3 . #2A((T NIL) (T T) (NIL T))))
 
 ((0 . #2A((T T) (T T))) (7 . #2A((T T T) (NIL NIL T)))
 (0 . #2A((NIL T) (T T) (NIL T))) (5 . #2A((NIL T) (T T) (T NIL)))
 (3 . #2A((T NIL) (T T) (NIL T))))