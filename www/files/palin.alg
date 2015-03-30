algoritmo "semnome"
// Função :
// Autor :
// Data : 4/22/2014
// Seção de Declarações 
var
   max, n1, n2, n, i, j, sz: inteiro
   palin, found: logico
   v: vetor [1..6] de inteiro
inicio
      max <- 0
      para n1 de 999 ate 100 passo -1 faca
           para n2 de n1 ate 100 passo -1 faca
                n <- n1 * n2
                sz <- 1
                enquanto n <> 0 faca
                         v[sz] <- n % 10
                         n <- n \ 10
                         sz <- sz + 1
                fimenquanto
                j <- sz - 1
                palin <- verdadeiro
                i <- 1
                enquanto palin e (i < j) faca
                         se  v[i] <> v[j] entao
                             palin <- falso
                         fimse
                         i <- i + 1
                         j <- j - 1
                fimenquanto
                se palin entao
                   n <- n1 * n2
                   se n > max entao
                      max <- n
                   fimse
                fimse
           fimpara
      fimpara
      escreva(max)
// Seção de Comandos 
fimalgoritmo
