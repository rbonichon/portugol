algoritmo "semnome"
funcao par (n : inteiro): logico
inicio
      se n = 0 entao
         retorne verdadeiro
      senao
         retorne (impar (n - 1))
         fimse
fimfuncao

funcao impar (n : inteiro) : logico
var x : logico
inicio
      se n = 0 entao
         retorne falso
      senao retorne (par (n - 1))
      fimse
fimfuncao

inicio
      escreva(par(5))
fimalgoritmo
