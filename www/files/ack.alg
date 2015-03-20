algoritmo "ack"
          funcao ack (m, n: inteiro): inteiro
          inicio
                se m = 0 entao retorne n + 1
                senao se n = 0 entao retorne ack (m - 1, 1)
                senao retorne ack (m - 1, ack (m, n - 1))
                fimse fimse
          fimfuncao
inicio
        escreva (ack (2, 1))
fimalgoritmo
