;;;;; Ficheiro interact.lisp

;;; Carrega os outros ficheiros de código, escreve e lê de ficheiros e trata da interação com o utilizador.

(load "C:/Users/joaoq/OneDrive/Faculdade/Inteligencia_artificial/Projeto_IA/projeto_IA_fase_2_23_24/algoritmo.lisp")
(load "C:/Users/joaoq/OneDrive/Faculdade/Inteligencia_artificial/Projeto_IA/projeto_IA_fase_2_23_24/jogo.lisp")

(defun jogar ()
    (let ((tipo-jogo (jogadores)) (tabuleiro (tabuleiro-aleatorio)) (profundidade (ler-profundidade)))
        (cond 
            ((equal tipo-jogo 'jogador-jogador) 
             (let* ((tabuleiro-novo (primeira-jogada (primeira-jogada tabuleiro T) nil))
                    (no (cria-no tabuleiro-novo 0 
                            (celula (nth 0 (procurar-posicao tabuleiro-novo -1)) (nth 1 (procurar-posicao tabuleiro-novo -1)) tabuleiro) 
                            (celula (nth 0 (procurar-posicao tabuleiro-novo -2)) (nth 1 (procurar-posicao tabuleiro-novo -2)) tabuleiro) 
                            nil nil 0 (procurar-posicao tabuleiro-novo -1) (procurar-posicao tabuleiro-novo -2))))
                (mostrar-jogada no)
                (funcall tipo-jogo no T)
            ))
            ((equal tipo-jogo 'ai-ai)
                (let* ((tabuleiro-novo (primeira-melhor-jogada (primeira-melhor-jogada tabuleiro t) nil))
                       (no (cria-no tabuleiro-novo 0 
                            (celula (nth 0 (procurar-posicao tabuleiro-novo -1)) (nth 1 (procurar-posicao tabuleiro-novo -1)) tabuleiro) 
                            (celula (nth 0 (procurar-posicao tabuleiro-novo -2)) (nth 1 (procurar-posicao tabuleiro-novo -2)) tabuleiro) 
                            nil nil 0 (procurar-posicao tabuleiro-novo -1) (procurar-posicao tabuleiro-novo -2))))
                (mostrar-jogada no)
                (funcall tipo-jogo no profundidade T)
            ))
            (t (let* ((tabuleiro-novo (primeira-melhor-jogada (primeira-jogada tabuleiro t) nil))
                      (no (cria-no tabuleiro-novo 0 
                            (celula (nth 0 (procurar-posicao tabuleiro-novo -1)) (nth 1 (procurar-posicao tabuleiro-novo -1)) tabuleiro) 
                            (celula (nth 0 (procurar-posicao tabuleiro-novo -2)) (nth 1 (procurar-posicao tabuleiro-novo -2)) tabuleiro) 
                            nil nil 0 (procurar-posicao tabuleiro-novo -1) (procurar-posicao tabuleiro-novo -2))))
                (mostrar-jogada no)
                (funcall tipo-jogo no profundidade T)
            ))
        )
    )
)

(defun jogadores ()
"Função que permite ler do ecrã que tipo de jogadores vão jogar"
    (progn 
        (format t "Escolher jogadores: ~%")
        (format t "1- AI vs AI~%")
        (format t "2- AI vs Jogador~%")
        (format t "3- Jogador vs Jogador~%")
        (case (read)
            (1 'ai-ai)
            (2 'ai-jogador)
            (3 'jogador-jogador)
        )
    )
)

(defun ler-profundidade()
"Permite fazer a leitura da profundidade limite."
    (progn
        (format t "Qual a profundidade limite? ~%")
        (read)
    )
)

(defun primeira-melhor-jogada (tabuleiro jogador)
"Função que permite escolher a melhor jogada dependendo se o jogador é do cavalo branco ou preto. 
Devolve o tabuleiro com o cavalo posicionado."
    (cond 
        (jogador (substituir 0 (position (apply #'max (car tabuleiro)) (car tabuleiro)) tabuleiro -1))
        (t (substituir 9 (position (apply #'max (nth 9 tabuleiro)) (nth 9 tabuleiro)) tabuleiro -2))
    )
)

(defun primeira-jogada (tabuleiro jogador)
"Função que permite ao utilizador humano escolher a sua primeira jogada no tabuleiro dependendo se este é cavalo preto ou branco."
    (print-tabuleiro tabuleiro)
    (cond 
        (jogador (progn
            (format t "Escolha onde quer começar:~%")
            (format t "1- 1A~%")
            (format t "2- 1B~%")
            (format t "3- 1C~%")
            (format t "4- 1D~%")
            (format t "5- 1E~%")
            (format t "6- 1F~%")
            (format t "7- 1G~%")
            (format t "8- 1H~%")
            (format t "9- 1I~%")
            (format t "10- 1J~%")
            (case (read)
                (1 (substituir 0 0 tabuleiro -1))
                (2 (substituir 0 1 tabuleiro -1))
                (3 (substituir 0 2 tabuleiro -1))
                (4 (substituir 0 3 tabuleiro -1))
                (5 (substituir 0 4 tabuleiro -1))
                (6 (substituir 0 5 tabuleiro -1))
                (7 (substituir 0 6 tabuleiro -1))
                (8 (substituir 0 7 tabuleiro -1))
                (9 (substituir 0 8 tabuleiro -1))                            
                (10 (substituir 0 9 tabuleiro -1))
            )
        ))
        (t (progn
            (format t "Escolha onde quer começar:~%")
            (format t "1- 10A~%")
            (format t "2- 10B~%")
            (format t "3- 10C~%")
            (format t "4- 10D~%")
            (format t "5- 10E~%")
            (format t "6- 10F~%")
            (format t "7- 10G~%")
            (format t "8- 10H~%")
            (format t "9- 10I~%")
            (format t "10- 10J~%")
            (case (read)
                (1 (substituir 9 0 tabuleiro -2))
                (2 (substituir 9 1 tabuleiro -2))
                (3 (substituir 9 2 tabuleiro -2))
                (4 (substituir 9 3 tabuleiro -2))
                (5 (substituir 9 4 tabuleiro -2))
                (6 (substituir 9 5 tabuleiro -2))
                (7 (substituir 9 6 tabuleiro -2))
                (8 (substituir 9 7 tabuleiro -2))
                (9 (substituir 9 8 tabuleiro -2))                            
                (10 (substituir 9 9 tabuleiro -2))
            )
        ))
    )   
)

(defun escolher-jogada (no jogador)
"Permite mostrar ao jogador (cavalo branco ou preto) as jogadas que pode realizar e dentre essas escolher uma.
Devolve o nó filho com a jogada realizada."
    (let ((jogadas (jogadas-possiveis no jogador)))
        (format t "Jogador ~d escolha uma jogada das disponiveis:~%" jogador)
        (cond 
            ((equal jogador 'max) (mapcar 
                #'(lambda (no-filho)
                    (format t "~a- ~a~a~%" (+ (position no-filho jogadas) 1)
                        (nth 0 (converter-posicao (no-posicao-max no-filho))) 
                        (nth 1 (converter-posicao (no-posicao-max no-filho))))
                )
                jogadas))
            (t (mapcar 
                #'(lambda (no-filho) (format t "~a- ~a~a~%" (+ (position no-filho jogadas) 1) 
                    (nth 0 (converter-posicao (no-posicao-min no-filho))) 
                    (nth 1 (converter-posicao (no-posicao-min no-filho))))
                ) 
            jogadas))
        )
        (nth (- (read) 1) jogadas)
    )
)

(defun jogador-jogador (no jogadormax)
"Função de jogo humano contra humano. Mostra no ecrã o nó de forma formatada quando não for possivel realizar mais jogadas."
    (cond 
        ((null (jogadas-possiveis no)) (progn (format t "Não há mais jogadas~%") (mostrar-jogada no)))
        (jogadormax                
            (progn 
                (let ((jogada (escolher-jogada no 'max)))
                    (mostrar-jogada jogada)
                    (jogador-jogador jogada nil)    
                )
            )
        )
        (t (progn 
            (let ((jogada (escolher-jogada no 'min)))
                (mostrar-jogada jogada)
                (jogador-jogador jogada T)    
            )
        ))
    )
)

(defun ai-ai (no profundidade jogadormax)
"Função de jogo de computador contra computador. Mostra no ecrã o nó de forma formatada quando não for possivel realizar mais jogadas."
    (cond 
        ((null (jogadas-possiveis no)) (progn (format t "Não há mais jogadas~%") (mostrar-jogada no)))
        (jogadormax (ai-ai (alfabeta no profundidade -9999 9999 jogadormax) profundidade nil))
        (t (ai-ai (alfabeta no profundidade -9999 9999 jogadormax) profundidade T))
    )
)

(defun ai-jogador (no profundidade jogadormax)
"Função de jogo de computador contra humano. Mostra no ecrã o nó de forma formatada quando não for possível realizar mais jogadas."
    (cond 
        ((null (jogadas-possiveis no)) (progn (format t "Não há mais jogadas~%") (mostrar-jogada no)))
        (jogadormax 
            (progn 
                (mostrar-jogada no)
                (ai-jogador (escolher-jogada no 'max) profundidade nil)
            )
        )
        (t (ai-jogador (alfabeta no profundidade -9999 9999 jogadormax) profundidade t))
    )
)