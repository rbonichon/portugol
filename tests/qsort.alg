algoritmo "quicksort"
var a : vetor [1..8] de inteiro
    i : inteiro
    c : real

funcao calcula_pivo(first_idx, last_idx: inteiro) : inteiro
inicio
       retorne ((first_idx + last_idx) \ 2)
fimfuncao

procedimento put_a ()
inicio
        escreval("")
        para i de 1 ate 8 faca
             escreva(a[i], " ")
        fimpara
fimprocedimento

procedimento qsort(first_idx, last_idx: inteiro)
var lt_idx, pividx: inteiro
    tmp : inteiro
    j : inteiro
inicio
        pividx <- calcula_pivo(first_idx, last_idx)
        se first_idx < last_idx entao
          tmp <- a[last_idx]
          a[last_idx] <- a[pividx]
          a[pividx] <- tmp
          lt_idx <- last_idx - 1
          j <- first_idx
          enquanto j <= lt_idx faca
               se a[j] > a[last_idx] entao
                  tmp <- a[j]
                  a[j] <- a[lt_idx]
                  a[lt_idx] <- tmp
                  lt_idx <- lt_idx - 1
               senao
                  j <- j + 1
               fimse
          fimenquanto
          tmp <- a[lt_idx + 1]
          a[lt_idx + 1] <- a[last_idx]
          a[last_idx] <- tmp
          qsort(first_idx, lt_idx)
          qsort(lt_idx + 2, last_idx)
        fimse
fimprocedimento

inicio
        para i de 1 ate 8 faca
             a[i] <- 8 - i
        fimpara
        put_a()
        qsort(1, 8)
        put_a()
fimalgoritmo
