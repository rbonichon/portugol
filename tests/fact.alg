algoritmo "fact"
funcao aux (n: inteiro) : inteiro
inicio
        se n = 0 entao retorne 1
        senao retorne (n * (aux (n -1 )))
        fimse
fimfuncao

inicio
        escreva(aux(5))
fimalgoritmo
