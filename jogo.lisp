;;;;; Ficheiro jogo.lisp do Projeto 2
;;; Autor: Jo�o Quintiliano, 201900287
;;; Curso: Licenciatura em Engenharia Inform�tica
;;; UC: Intelig�ncia Artificial

;;; C�digo relacionado com o problema.

;;;Ficheiro puzzle.lisp
;;Ficheiro com operadores e heur�sticas espec�ficos do dom�nio da aplica��o


(defun tabuleiro ()
"Tabuleiro de teste igual ao anterior mas tendo sido colocado o cavalo na posi��o: i=0 e j=0"
  '(
    (3 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 49 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 -1 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 -2)
    )
)


;;; M�todos seletores
(defun linha (indice tabuleiro)
"Fun��o que recebe um �ndice e o tabuleiro e retorna uma lista que representa essa linha do
tabuleiro."
    (cond ((and (integerp indice) (>= indice 0) (< indice (length tabuleiro))) 
            (nth indice tabuleiro))
          (t nil)
    )
)

(defun celula (indiceLinha indiceColuna tabuleiro)
"Fun��o que recebe dois �ndices e o tabuleiro e retorna o valor presente nessa c�lula do
tabuleiro."
    (cond ((and (integerp indiceColuna) (>= indiceColuna 0) (< indiceColuna (length (linha indiceLinha tabuleiro)))) 
            (nth indiceColuna (linha indiceLinha tabuleiro)))
          (t nil)
    )
)

(defun lista-numeros (&optional (n 100))
"Fun��o que recebe um n�mero positivo n e cria uma lista com todos os n�meros
entre 0 (inclusiv�) e o n�mero passado como argumento (exclusiv�). Por default o n � 100"
    ;(cond ((= n 0) nil)
     ;     (t (cons (- n 1) (lista-numeros (- n 1)))))
'(99 98 97 96 95 94 93 92 91 90
 89 88 87 86 85 84 83 82 81 80
 79 78 77 76 75 74 73 72 71 70
 69 68 67 66 65 64 63 62 61 60
 59 58 57 56 55 54 53 52 51 50
 49 48 47 46 45 44 43 42 41 40
 39 38 37 36 35 34 33 32 31 30
 29 28 27 26 25 24 23 22 21 20
 19 18 17 16 15 14 13 12 11 10
 9 8 7 6 5 4 3 2 1 0)
)

(defun numero-aleatorio (lista)
"Fun��o que recebe uma lista e retorna um n�mero aleat�rio dessa lista"
    (cond 
        ((= (length lista) 0) 0)
        (t (nth (random (length lista)) lista))
    )
)

(defun baralhar (remover &optional (nova-lista '()) (num (numero-aleatorio remover)))
"Fun��o que recebe um tabuleiro e baralha esse tabuleiro"
    (cond 
        ((= (length remover) 0) nova-lista)
        (T (baralhar (remove-if #'(lambda (x) (= x num)) remover) (cons num nova-lista)))
    ) 
)

(defun tabuleiro-aleatorio (&optional (lista (baralhar (lista-numeros))) (n 10))
"Recebe uma lista (por default ser� chamada a fun��o baralhar com argumento a fun��o lista-numeros)
e vai devolver essa lista dividida em sublistas de n elementos recebido como par�metro pelo argumento n
(por default ter� o valor 10)"
    (cond ((null lista) nil)
          (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))
    )
)

(defun substituir (indice-linha indice-coluna tabuleiro &optional (novo-numero 'nil))
  "Substitui o n�mero em uma posi��o espec�fica do tabuleiro."
    (let ((novo-tabuleiro (substituir-resto tabuleiro indice-linha (substituir-linha (nth indice-linha tabuleiro) indice-coluna novo-numero))))
        (cond 
            ((not (equal novo-numero 'nil))
                (cond 
                    ((isduplo (celula indice-linha indice-coluna tabuleiro))
                        (cond 
                            ((maior-duplo novo-tabuleiro) (substituir-duplo novo-tabuleiro))
                            (t novo-tabuleiro)
                        )
                    )
                    (t (substituir-simetrico (celula indice-linha indice-coluna tabuleiro) novo-tabuleiro))
                )
            )
            (t novo-tabuleiro)
        )
    )
)

(defun substituir-linha (linha indice-coluna novo-numero)
"Substitui o n�mero em uma posi��o espec�fica da linha."
    (cond
        ((null linha) nil)
        ((= indice-coluna 0) (cons novo-numero (cdr linha)))
        (t (cons (car linha) (substituir-linha (cdr linha) (1- indice-coluna) novo-numero)))
    )
)

(defun substituir-resto (tabuleiro indice-linha nova-linha)
"Retorna o resto do tabuleiro sem modificar a linha especificada."
    (cond
        ((null tabuleiro) nil)
        ((= indice-linha 0) (cons nova-linha (cdr tabuleiro)))
        (t (cons (car tabuleiro) (substituir-resto (cdr tabuleiro) (1- indice-linha) nova-linha)))
    )
)


(defun substituir-simetrico (numero tabuleiro)
"Fun��o que recebe um n�mero, um tabuleiro. A fun��o vai retornar o tabuleiro 
com o sim�trico do n�mero da vari�vel numero substituido por NIL"
    (cond 
        ((null (procurar-posicao tabuleiro (obter-simetrico numero))) tabuleiro)
        (t (substituir (nth 0 (procurar-posicao tabuleiro (obter-simetrico numero))) (nth 1 (procurar-posicao tabuleiro (obter-simetrico numero))) tabuleiro))
    )
)

(defun obter-simetrico (numero)
"Fun��o que recebe um n�mero e devolve o sim�trico desse n�mero (exemplo: 56->65)"
  (parse-integer (coerce (reverse (coerce (princ-to-string numero) 'list)) 'string))
)

(defun substituir-duplo (tabuleiro)
"Recebe um tabuleiro e substitui o maior duplo do tabuleiro"
    (cond 
        ((null (maior-duplo tabuleiro)) nil)
        (t (substituir (nth 0 (procurar-posicao tabuleiro (maior-duplo tabuleiro))) (nth 1 (procurar-posicao tabuleiro (maior-duplo tabuleiro))) tabuleiro))
    )
)

(defun maior-duplo (tabuleiro &optional (duplos '()))
"Fun��o que recebe um tabuleiro e retorna o duplo maior desse tabuleiro"
    (cond
        ((null tabuleiro) (cond ((null duplos) nil) (t (apply #'max duplos)))) 
        ((null (remove-if-not #'isduplo (car tabuleiro))) (maior-duplo (cdr tabuleiro) duplos))
        (t (maior-duplo (cdr tabuleiro) (append (remove-if-not #'isduplo (car tabuleiro)) duplos)))
    )
)

(defun isduplo (numero)
"Recebe um n�mero e retorna verdadeiro (T) se o n�mero for um duplo, retorna nil caso contr�rio"
    (cond 
        ((null numero) nil)
        ((not (integerp numero)) nil)
        ((= (mod numero 11) 0) T)
        (t nil)
    )
)

(defun procurar-posicao (tabuleiro valor &optional (valor-linha 0))
"Fun��o que recebe o tabuleiro e devolve a posi��o (i j) em que se encontra o
valor passado por argumento. Pode ser usado para procurar o cavalo.
Caso o valor n�o se encontre no tabuleiro retorna NIL."
    (cond 
        ((>= valor-linha (length tabuleiro)) nil)
        ((let ((lista-linha (linha valor-linha tabuleiro)))
            (cond 
                ((find valor lista-linha) (cons valor-linha (cons (position valor lista-linha) nil)))
                (t (procurar-posicao tabuleiro valor (+ valor-linha 1)))
            )
        ))
    )
)

(defun operadores ()
 "Cria uma lista com todos os operadores do problema."
 (list 'operador-1 'operador-2 'operador-3 'operador-4 'operador-5 'operador-6 'operador-7 'operador-8))


;;; Operadores do problema
;; operador
(defun operador (tabuleiro nova-pos-linha nova-pos-coluna posicao-cavalo jogadormax)
"Fun��o que recebe um tabuleiro. Realiza um movimento do cavalo 2 casas para baixo e 1 para a esquerda.
Devolve o tabuleiro com a nova posi��o do cavalo."
    (cond ((or (null nova-pos-linha) (null nova-pos-coluna)) nil)
        (t (cond 
            ((or (>= nova-pos-linha (length (car tabuleiro))) (>= nova-pos-coluna (length tabuleiro)) (< nova-pos-linha 0) (< nova-pos-coluna 0)) 
                nil)
            ((or (equal (celula nova-pos-linha nova-pos-coluna tabuleiro) 'nil) 
                 (equal (celula nova-pos-linha nova-pos-coluna tabuleiro) -1) 
                 (equal (celula nova-pos-linha nova-pos-coluna tabuleiro) -2)) nil)
            (t (cond
                    ((null (substituir (nth 0 posicao-cavalo) (nth 1 posicao-cavalo) (substituir nova-pos-linha nova-pos-coluna tabuleiro (if jogadormax -1 -2)))) nil)
                    (t (substituir (nth 0 posicao-cavalo) (nth 1 posicao-cavalo) (substituir nova-pos-linha nova-pos-coluna tabuleiro (if jogadormax -1 -2))))
                )
            )
        ))
    )  
)

(defun operador-1 (tabuleiro jogadorMax)
"Movimento 1 do cavalo, 2 casa para baixo e 1 para a esquerda"
    (cond 
        ((null (procurar-posicao tabuleiro (if jogadormax -1 -2))) nil)
        (t (operador 
                tabuleiro 
                (+ (nth 0 (procurar-posicao tabuleiro (if jogadormax -1 -2))) 2) 
                (- (nth 1  (procurar-posicao tabuleiro (if jogadormax -1 -2))) 1) 
                (procurar-posicao tabuleiro (if jogadormax -1 -2))
                jogadormax
            ))
    )
)

(defun operador-2 (tabuleiro jogadorMax)
"Movimento 2 do cavalo, 2 casa para baixo e 1 para a direita"
    (cond 
        ((null (procurar-posicao tabuleiro (if jogadormax -1 -2))) nil)
        (t (operador 
                tabuleiro 
                (+ (nth 0 (procurar-posicao tabuleiro (if jogadormax -1 -2))) 2) 
                (+ (nth 1  (procurar-posicao tabuleiro (if jogadormax -1 -2))) 1) 
                (procurar-posicao tabuleiro (if jogadormax -1 -2))
                jogadormax
            ))
    )
)

(defun operador-3 (tabuleiro jogadorMax)
"Movimento 3 do cavalo, 2 casa para a direita e 1 para baixo"
    (cond 
        ((null (procurar-posicao tabuleiro (if jogadormax -1 -2))) nil)
        (t (operador 
                tabuleiro 
                (+ (nth 0 (procurar-posicao tabuleiro (if jogadormax -1 -2))) 1) 
                (+ (nth 1  (procurar-posicao tabuleiro (if jogadormax -1 -2))) 2) 
                (procurar-posicao tabuleiro (if jogadormax -1 -2))
                jogadormax
            ))
    )
)

(defun operador-4 (tabuleiro jogadorMax)
"Movimento 4 do cavalo, 2 casa para a direita e 1 para cima"
    (cond 
        ((null (procurar-posicao tabuleiro (if jogadormax -1 -2))) nil)
        (t (operador 
                tabuleiro 
                (- (nth 0 (procurar-posicao tabuleiro (if jogadormax -1 -2))) 1) 
                (+ (nth 1  (procurar-posicao tabuleiro (if jogadormax -1 -2))) 2) 
                (procurar-posicao tabuleiro (if jogadormax -1 -2))
                jogadormax
            ))
    )
)

(defun operador-5 (tabuleiro jogadorMax)
"Movimento 5 do cavalo, 2 casa para cima e 1 para a direita"
    (cond 
        ((null (procurar-posicao tabuleiro (if jogadormax -1 -2))) nil)
        (t (operador 
                tabuleiro 
                (- (nth 0 (procurar-posicao tabuleiro (if jogadormax -1 -2))) 2) 
                (+ (nth 1  (procurar-posicao tabuleiro (if jogadormax -1 -2))) 1) 
                (procurar-posicao tabuleiro (if jogadormax -1 -2))
                jogadormax
            ))
    )
)

(defun operador-6 (tabuleiro jogadorMax)
"Movimento 6 do cavalo, 2 casa para cima e 1 para a esquerda"
    (cond 
        ((null (procurar-posicao tabuleiro (if jogadormax -1 -2))) nil)
        (t (operador 
                tabuleiro 
                (- (nth 0 (procurar-posicao tabuleiro (if jogadormax -1 -2))) 2) 
                (- (nth 1  (procurar-posicao tabuleiro (if jogadormax -1 -2))) 1) 
                (procurar-posicao tabuleiro (if jogadormax -1 -2))
                jogadormax
            ))
    )
)

(defun operador-7 (tabuleiro jogadorMax)
"Movimento 7 do cavalo, 2 casa para a esquerda e 1 para cima"
    (cond 
        ((null (procurar-posicao tabuleiro (if jogadormax -1 -2))) nil)
        (t (operador 
                tabuleiro 
                (- (nth 0 (procurar-posicao tabuleiro (if jogadormax -1 -2))) 1) 
                (- (nth 1  (procurar-posicao tabuleiro (if jogadormax -1 -2))) 2) 
                (procurar-posicao tabuleiro (if jogadormax -1 -2))
                jogadormax
            ))
    )
)

(defun operador-8 (tabuleiro jogadorMax)
"Movimento 8 do cavalo, 2 casa para a esquerda e 1 para baixo"
    (cond 
        ((null (procurar-posicao tabuleiro (if jogadormax -1 -2))) nil)
        (t (operador 
                tabuleiro 
                (+ (nth 0 (procurar-posicao tabuleiro (if jogadormax -1 -2))) 1) 
                (- (nth 1  (procurar-posicao tabuleiro (if jogadormax -1 -2))) 2) 
                (procurar-posicao tabuleiro (if jogadormax -1 -2))
                jogadormax
            ))
    )
)

; no do problema
; - tabuleiro
; - profundidade do n� na arvore
; - pontua��o do jogador max
; - pontua��o do jogador min
; - operadores do jogador max
; - operadores do jogador min
; - valor do n�
; - posi��o do cavalor do jogador max no tabuleiro
; - posi��o do cavalor do jogador min no tabuleiro
 
;;; Construtor
(defun cria-no (tabuleiro profundidade pontuacao-max pontuacao-min operadores-max operadores-min valor &optional (pos-max nil) (pos-min nil))
"Fun��o que cria um n� do problema"
  (list tabuleiro profundidade pontuacao-max pontuacao-min operadores-max operadores-min valor pos-max pos-min)
)

(defun no-mudar-valor (no valor)
"Recebe um n� e um valor. Altera o valor do n�."
    (cria-no (no-tabuleiro no) (no-profundidade no) (no-pontuacao-max no) (no-pontuacao-min no) (no-operadores-max no) (no-operadores-min no) valor)
)

;;;; Metodos seletores
(defun no-tabuleiro (no)
"Retorna o tabuleiro do n�"
   (nth 0 no) 
)

(defun no-profundidade (no)
"Retorna a profundidade do n�"
    (nth 1 no)
)

(defun no-pontuacao-max (no)
"Retorna a pontua��oo do jogador max do n�"
    (nth 2 no)
)

(defun no-pontuacao-min (no)
"Retorna a pontua��o do jogador min do n�"
    (nth 3 no)
)

(defun no-operadores-max (no)
"Retorna os operadores realizados pelo jogador max no n�"
    (nth 4 no)
)

(defun no-operadores-min (no)
"Retorna os operadores realizados pelo jogador min no n�"
    (nth 5 no)
)

(defun no-valor (no)
"Retorna o valor do n�"
    (nth 6 no)
)

(defun no-posicao-max (no)
"Retorna a posi��o do cavalo do jogador max no tabuleiro do n�"
    (nth 7 no)
)

(defun no-posicao-min (no)
"Retorna a posi��o do cavalo do jogador min no tabuleiro do n�"
    (nth 8 no)
)

(defun novo-sucessor (no operador jogadorMax)
"Recebe um no, um operador de jogo e para que jogador vai ser feito o operador. Devolve um n�-filho descendente do n�."
    (let* ((tabuleiro (funcall operador (no-tabuleiro no) jogadormax))
          (valor-celula (celula (nth 0 (procurar-posicao tabuleiro (if jogadormax -1 -2))) (nth 1 (procurar-posicao tabuleiro (if jogadormax -1 -2))) (no-tabuleiro no))))
        (cond
            ((null tabuleiro) (cria-no (no-tabuleiro no) (+ (no-profundidade no) 1) 
                                       (no-pontuacao-max no) (no-pontuacao-min no) 
                                       (no-operadores-max no) (no-operadores-min no) 
                                       (no-valor no) (procurar-posicao (no-tabuleiro no) -1) (procurar-posicao (no-tabuleiro no) -2)))
            (jogadorMax (cria-no tabuleiro (+ (no-profundidade no) 1) 
                                (+ (no-pontuacao-max no) valor-celula) 
                                (no-pontuacao-min no) (append (no-operadores-max no) (list operador)) 
                                (no-operadores-min no) (no-valor no) 
                                (procurar-posicao tabuleiro -1) (procurar-posicao tabuleiro -2)))
            (t (cria-no tabuleiro (+ (no-profundidade no) 1) 
                        (no-pontuacao-max no) (+ (no-pontuacao-min no) valor-celula)
                        (no-operadores-max no)(append (no-operadores-min no) (list operador)) 
                        (no-valor no) (procurar-posicao tabuleiro -1) (procurar-posicao tabuleiro -2)))
        )   
    )
)

(defun sucessores (no lista-operadores jogadormax)
"Retorna todos os sucessores do n� passado por par�metro conforme se o jogador � o max ou min."
    (cond 
        ((null lista-operadores) nil)
        (t (cons (novo-sucessor no (car lista-operadores) jogadormax) (sucessores no (cdr lista-operadores) jogadormax)))
    )
)

(defun jogadas-possiveis (no &optional (tipo-jogador nil))
"Fun��o que verifica se algum dos jogadores ainda consegue realizar jogadas"
    (cond
        ((null tipo-jogador) (append 
            (remove-if-not #'(lambda (no-filho) (if (equal (no-tabuleiro no-filho) (no-tabuleiro no)) nil t)) (sucessores no (operadores) T)) 
            (remove-if-not #'(lambda (no-filho) (if (equal (no-tabuleiro no-filho) (no-tabuleiro no)) nil t)) (sucessores no (operadores) nil))))
        ((null (procurar-posicao (no-tabuleiro no) (if (equal tipo-jogador 'max) -1 -2))) nil)
        ((equal tipo-jogador 'max) (remove-if-not #'(lambda (no-filho) (if (equal (no-tabuleiro no-filho) (no-tabuleiro no)) nil t)) (sucessores no (operadores) T)))
        (t (remove-if-not #'(lambda (no-filho) (if (equal (no-tabuleiro no-filho) (no-tabuleiro no)) nil t)) (sucessores no (operadores) nil)))
    )
)

(defun converter-posicao (posicao)
"Fun��o que recebe uma posi��o do tabuleiro e converte nas coordenadas do tabuleiro (exemplo 00 = A1)"
    (list (+ (nth 0 posicao) 1) (code-char (+ (nth 1 posicao) 65)))    
)

(defun print-tabuleiro (tabuleiro &optional (linha 0))
"Recebe um tabuleiro e imprime o tabuleiro no ecr� de forma formatada"
    (cond 
        ((null tabuleiro) nil)
        ((= linha 0) (progn 
                (format t "   ")
                (mapcar #'(lambda (elemento) (format t " ~3a " elemento)) '(A B C D E F G H I J)) 
                (terpri) (print-tabuleiro tabuleiro (+ linha 1))))
        (t (progn
            (format t "~2a|" linha)
            (mapcar #'(lambda (elemento) (format t " ~3a " elemento)) (car tabuleiro))
            (format t "~%")
            (print-tabuleiro (cdr tabuleiro) (+ linha 1)) 
           )
        )
    )
)

(defun escrever-no (no)
 "Permite escrever um n�."
    (format t "Tabuleiro ~%")
    (print-tabuleiro (no-tabuleiro no))
    (format t " | Profundidade: ~a~% | Pontos-max: ~a~% | Pontos-min: ~a~% | Operadores-max: ~a~% | Operadores-min: ~a~% | Valor: ~a~% | posicao-max: ~a~% | posicao-min: ~a~% " 
            (no-profundidade no) (no-pontuacao-max no) (no-pontuacao-min no) (no-operadores-max no) (no-operadores-min no) (no-valor no) (converter-posicao (no-posicao-max no)) (converter-posicao (no-posicao-min no)))
    (format t "-----------------------------------------------------~%")
)

(defun escrever-nos (lista)
"Permite escrever no ecr� v�rios n�s."
    (cond 
        ((null lista) nil)
        (t (progn
            (escrever-no (car lista))
            (escrever-nos (cdr lista))
            )
        )
    )
)

(defun mostrar-jogada (no)
"Permite mostrar a informa��o relativa a uma jogada."
     (format t "Tabuleiro ~%")
    (print-tabuleiro (no-tabuleiro no))
    (format t " | Pontos-jogador-1: ~a~% | Pontos-jogador-2: ~a~% | posicao-max: ~a~% | posicao-min: ~a~%"
            (no-pontuacao-max no) (no-pontuacao-min no) (converter-posicao (no-posicao-max no)) (converter-posicao (no-posicao-min no)))
    (format t "-----------------------------------------------------~%")
)

(defun escrever-dados-procura (lista)
    (format t "Tabuleiro~%")
    (print-tabuleiro (no-tabuleiro (car (car lista))))
    (format t "  | Pontos-jogador-1: ~a~% | Pontos-jogador-2: ~a~% | posicao-max: ~a~% | posicao-min: ~a~% | Tempo de execu��o: ~a~%" 
            (no-pontuacao-max (car (car lista))) 
            (no-pontuacao-min (car (car lista))) 
            (converter-posicao (no-posicao-max (car (car lista)))) 
            (converter-posicao (no-posicao-min (car (car lista))))
            (car (cdr lista)))
    (format t "-----------------------------------------------------~%")
)




