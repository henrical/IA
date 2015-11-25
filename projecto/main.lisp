(setf custo #'custo-oportunidade)
(setf heur #'qualidade)

(setf s1 (create-stack))
(stack-ordered-push! s1 e1 custo heur)
(stack-ordered-push! s1 e2 custo heur)
(stack-ordered-push! s1 e3 custo heur)
(stack-ordered-push! s1 e4 custo heur)

(stack-pop! s1) 
;; retorna e1

(stack-ordered-push! s1 e1 custo heur)
(stack-ordered-push! s1 e2 custo heur)
(stack-ordered-push! s1 e3 custo heur)

(stack-pop! s1) 
;; retorna e1

(stack-pop! s1) 
;; retorna e3

(stack-pop! s1) 
;; retorna e3

(stack-pop! s1) 
;; retorna e2

(stack-pop! s1) 
;; retorna e2

(stack-pop! s1) 
;; retorna e4