algoritmo "pot"
          funcao pot(x: real; n: inteiro): real
          inicio
                se n = 0 entao retorne 1
                senao retorne x * pot (x, n - 1)
                fimse
          fimfuncao
inicio
        escreva(pot(3, 3))
fimalgoritmo
