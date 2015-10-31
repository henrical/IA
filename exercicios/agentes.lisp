;;pratica 4

;;ex.1
(defstruct percepcao lixo-p toque-p)

(defun agente-reflexos-simples(p)
	(cond ((percepcao-lixo-p p) 'aspirar)
		((percepcao-toque-p p) 'rodar)
		(t 'andar)
	)
)

;; (setf percepcao1 (make-percepcao :lixo-p "nao" :toque-p "sim"))

;;ex2
(defstruct percepcao2 lixo-p)

(defun faz-agente-aspirador ()
	(let ((casa-actual 1) (virado-para-parede nil))
		#'(lambda (p)
			(cond ((percepcao2-lixo-p p) 'aspirar)
				((= casa-actual 4) 'esperar)
				(virado-para-parede (setf virado-para-parede nil) 'rodar)
				(t (incf casa-actual)
				   (setf virado-para-parede t)
				   'andar
				)
			)
		)
	)
)