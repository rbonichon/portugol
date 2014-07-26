
funcao par (n : inteiro): logico
inicio
      esc(m)
      se n = 0 entao
         retorne verdadeiro
      senao
         retorne (impar (n - 1))
         fimse
fimfuncao


procedimento esc(n:inteiro)
inicio
      escreval("Valor de n: ", n)
fimprocedimento

funcao impar (n : inteiro) : logico
var x : logico
inicio
      se n = 1 entao
         retorne (verdadeiro)
      senao retorne (par (n - 1))
      fimse
fimfuncao



algoritmo "semnome"
funcao ex (n:inteiro; var m: inteiro): inteiro
var tmp: inteiro
inicio
      tmp <- m
      m <- n
      esc(tmp)
      retorne tmp
fimfuncao






var m, n:inteiro
inicio
      escreva(par(5))
      
      n <- 5
      m <- 7
      escreva(n, m)
      n <- ex(n, m)
      escreva(n, m)
      esc(n)
// Seção de Comandos 
fimalgoritmo
