algoritmo "escolha_test"
var i, j: inteiro
inicio
        para i de 1 ate 10 faca
        j <- randi(i)
        escreval (j)
        escolha j
        caso 1, 9, 7
             escreval("foo")
        caso 2, 4, 5
             escreval("bar")
        outrocaso
             escreval("baz")
        fimescolha
        fimpara
fimalgoritmo
