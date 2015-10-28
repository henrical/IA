;;pratica 4

;;ex.1
(defstruct percepcao lixo-p toque-p)

(defun agente-reflexos-simples(p)
	(cond ((percepcao-lixo-p p) 'aspirar)
		((percepcao-toque-p p) 'rodar)
		(t 'andar)
	)
)

(setq percepcao1 (make-percepcao :lixo-p "nao" :toque-p "sim"))