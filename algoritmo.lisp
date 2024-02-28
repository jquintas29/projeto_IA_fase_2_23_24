;;;; Ficheiro algoritmo.lisp do Projeto 2
;;; Autor: João Quintiliano, 201900287
;;; Curso: Licenciatura em Engenharia Informática
;;; UC: Inteligência Artificial

;;; Deve conter a implementação do algoritmo de jogo independente do domínio.

;; Procura quiescente – a função de avaliação só é aplicada a posições com baixa probabilidade 
;; de terem variações bruscas no valor da função de avaliação. As posições não-quiescentes podem 
;; ser expandidas até se atingirem posições quiescentes (posições com baixa probabilidade de 
;; ter alterações bruscas no valor da função de avaliação).

; TESTAR
(defun avaliacao (no)
"Função de avaliação que mostra a diferença de pontos entre os jogadores.
Se o Max ganhar o valor será positivo, se o Min ganhar o valor será negativo. 
Se empatarem o valor será 0."
    (no-mudar-valor no (- (no-pontuacao-max no) (no-pontuacao-min no)))
)

(defun lista-max-value (lista &optional (no-max-value (cria-no nil 0 0 0 nil nil -9999)))
"Permite obter de uma lista de nós o nó com o valor maior."
    (cond 
        ((null lista) no-max-value)
        ((> (no-valor (car lista)) (no-valor no-max-value)) (lista-max-value (cdr lista) (car lista)))
        (t (lista-max-value (cdr lista) no-max-value))
    )
)

(defun lista-min-value (lista &optional (no-min-value (cria-no nil 0 0 0 nil nil 9999)))
"Permite obter de uma lista de nós o nó com o valor menor."
    (cond 
        ((null lista) no-min-value)
        ((< (no-valor (car lista)) (no-valor no-min-value)) (lista-min-value (cdr lista) (car lista)))
        (t (lista-min-value (cdr lista) no-min-value))
    )
)

(defun quicksort (lista asc)
"recebe uma lista de nós e devolve a lista organizada de forma ascendente ou descendente."
    (cond (asc (sort lista #'(lambda (x y) (> (no-valor x) (no-valor y)))))
          (t (sort lista #'(lambda (x y) (< (no-valor x) (no-valor y))))))
)

; (defun tempo-execucao (tipo-jogo no profundidade jogadormax)
;     (let ((inicio (get-internal-real-time)))
;         (list (funcall tipo-jogo no profundidade jogadormax) (* (- (get-internal-real-time) inicio) 1000))
;     )
; )

;; Adicionar limite de tempo (get-internal-real-time)
(defun alfabeta (no profundidade alfa beta jogadorMax)
"Função alfabeta. Retorna o nó da melhor jogada."
    (cond 
        ((or (null (jogadas-possiveis no)) (= profundidade 0)) (avaliacao no))
        (jogadorMax
            (cond 
                ((null (jogadas-possiveis no 'max)) no)
                (t (let ((bestvalue-max (cria-no nil 0 0 0 nil nil -99999)))
                    (lista-max-value (mapc
                        #'(lambda (no-filho)
                            (let* ((valor (alfabeta no-filho (- profundidade 1) alfa beta nil))
                                ( bestvalue-max (lista-max-value (list bestvalue-max valor)))
                                ( alfa (max alfa (no-valor bestvalue-max))))
                                (cond ((<= beta alfa) nil)
                                      (t valor))
                            )
                        )
                        (quicksort (jogadas-possiveis no 'max) t))
                    )
                ))
            )
        )
        (t (cond ((null (jogadas-possiveis no 'min)) no)
              (t (let ((bestvalue-min (cria-no nil 0 0 0 nil nil 99999)))
                    (lista-min-value (mapc
                        #'(lambda (no-filho)
                            (let* ((valor (alfabeta no-filho (- profundidade 1) alfa beta T))
                                ( bestvalue-min (lista-min-value (list bestvalue-min valor)))
                                ( beta (min beta (no-valor bestvalue-min))))
                                (cond ((<= beta alfa) nil)
                                      (t valor))
                            )
                        )
                        (quicksort (jogadas-possiveis no 'min) nil))
                    )
                ))
            )
        )
    )
)

