algoritmo "nqueens"
var n : inteiro
    s : vetor [1..20] de inteiro

procedimento read_posint(var i : inteiro; lim: inteiro)
var k : inteiro
inicio
  repita
   escreva("Entre com um inteiro entre 0 e ", lim, ": ")
   leia(k)
  ate (k > 0) e (k <= lim)
  i <- k
fimprocedimento


funcao is_free(rank : inteiro; col : inteiro): logico
var ok : logico
    r, c : inteiro
inicio
   ok <- verdadeiro
   r <- 1
   enquanto (r <= rank) faca
     c <- s[r]
     ok <- ok e
           (abs(rank - r) <> abs(col - c)) e
           (col <> c)
     r <- r + 1
   fimenquanto
   retorne ok
fimfuncao

procedimento handle()
var i : inteiro
inicio
  para i de 1 ate n faca
   escreva(i, ": ", s[i], "; ")
  fimpara
  escreval("")
fimprocedimento

procedimento place_queen(queen : inteiro)
var col : inteiro
inicio
   se (queen > n) entao
      handle()
   senao
     para col de 1 ate n faca
       se (is_free(queen, col)) entao
         s[queen] <- col
         place_queen(queen + 1)
         s[queen] <- 0
      fimse
    fimpara
   fimse
fimprocedimento

inicio
  read_posint(n, 20)
  place_queen(1)
fimalgoritmo
