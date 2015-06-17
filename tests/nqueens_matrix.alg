algoritmo "N-queens : matrix version"
var checker : vetor [1..20, 1..20] de logico
    i, j, n : inteiro

procedimento init_checker(c : vetor [1..20, 1..20] de logico;
                          sz : inteiro)
var i, j : inteiro
inicio
  para i de 1 ate sz faca
    para j de 1 ate sz faca
      c[i, j] <- verdadeiro // posicao livre
    fimpara
  fimpara
fimprocedimento

procedimento read_posint(var i : inteiro; lim: inteiro)
var k : inteiro
inicio
  repita
   escreva("Entre com um inteiro entre 1 e ", lim, ": ")
   leia(k)
  ate (k > 0) e (k <= lim)
  i <- k
fimprocedimento

// Copiar m1 em m2
procedimento copy_checker(m1, m2: vetor [1..20, 1..20] de logico; sz: inteiro)
var i, j : inteiro
inicio
  para i de 1 ate sz faca
    para j de 1 ate sz faca
      m2[i, j] <- m1[i, j]
    fimpara
  fimpara
fimprocedimento

// Marca uma dama na linha + coluna
// Propaga restrições
procedimento mark_queen(c : vetor [1..20, 1..20] de logico;
                        line, col, sz:inteiro)
var i, j : inteiro
inicio
  // Marcar coluna
  para i de 1 ate sz faca
    c[line, i] <- falso
    c[i, col] <- falso
  fimpara
  c[line, col] <- verdadeiro // Re-marcar posicao da dama

  // Marcar diagonal direita
  i <- line + 1
  j <- col + 1
  enquanto i >= 1 e j >= 1 e i <= sz e j <= sz faca
      c[i, j] <- falso
      i <- i + 1
      j <- j + 1
  fimenquanto

  // Marcar diagonal esquerda
  i <- line + 1
  j <- col - 1
  enquanto i >= 1 e j >= 1 e i <= sz e j <= sz faca
      c[i, j] <- falso
      i <- i + 1
      j <- j - 1
  fimenquanto
fimprocedimento

procedimento print_checker(c:vetor [1..20, 1..20] de logico;
                           sz: inteiro)
var i, j : inteiro
inicio
   para i de 1 ate sz faca
      j <- 1
      enquanto nao c[i, j] faca
         j <- j + 1
      fimenquanto
      escreva(i, ":", j, "; ")
   fimpara
   escreval("")
fimprocedimento

procedimento print_fullchecker(c:vetor [1..20, 1..20] de logico;
                              sz: inteiro)
var i, j : inteiro
inicio
   escreval("")
   para i de 1 ate sz faca
     para j de 1 ate sz faca
       se c[i, j] entao
         escreva(1)
       senao
         escreva(0)
       fimse
     fimpara
     escreval("")
   fimpara
   escreval("")
fimprocedimento

procedimento place_queen(c:vetor [1..20, 1..20] de logico;
                         sz, dama : inteiro)
var i : inteiro
    c2: vetor [1..20, 1..20] de logico
inicio
   se dama > sz entao
      print_checker(c, sz)
   senao
     para i de 1 ate sz faca
       se c[dama, i] entao
          copy_checker(c, c2, sz)
          mark_queen(c2, dama, i, sz)
          print_fullchecker(c2, sz)
          place_queen(c2, sz, dama + 1)
       fimse
     fimpara
   fimse
fimprocedimento

inicio
  read_posint(n, 20)
  init_checker(checker, n)
  place_queen(checker, n, 1)
fimalgoritmo
