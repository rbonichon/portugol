algoritmo "f1 test"
    funcao f(n: inteiro): inteiro
    inicio
    se n < 4 entao retorne (3 * n)
    senao retorne (2 * f(n - 4) + 5)
    fimse
    fimfuncao

inicio
    escreva(f(4))
fimalgoritmo
