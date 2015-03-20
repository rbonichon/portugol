
algoritmo "semnome"

     funcao par (n :inteiro): logico
inicio
      retorne ((n = 0) ou (impar (n - 1))
fimfuncao

funcao impar (n : inteiro) : logico
       retorne ((n = 1) ou (par (n - 1)))
fimfuncao

inicio
      escreva(par(5))
// Seção de Comandos 
fimalgoritmo
