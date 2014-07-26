algoritmo "mergesort"
var a, t : vetor [1..8] de inteiro
    i : inteiro

funcao calcula_media(first_idx, last_idx: inteiro) : inteiro
inicio
       retorne ((first_idx + last_idx + 1) \ 2)
fimfuncao

procedimento put_a ()
inicio
        escreval("")
        para i de 1 ate 8 faca
             escreva(a[i], " ")
        fimpara
fimprocedimento

procedimento copytotemp(first_idx, last_idx: inteiro)
inicio
        para i de first_idx ate last_idx faca
             t[i] <- a[i]
        fimpara
fimprocedimento



procedimento msort (first_idx, last_idx: inteiro)
var j, k: inteiro
    m: inteiro
    cond: logico
inicio
        put_a()
        se first_idx < last_idx entao
           m <- calcula_media(first_idx, last_idx)
           escreval("M =", m, first_idx, last_idx)
           // sort the two subpart of the array
           msort(m, last_idx)
           msort(first_idx, m - 1)
           // save in temporary array
           copytotemp(first_idx, last_idx)
           // reorganize from the two copies ordered sub-array
           // into the original array
           j <- first_idx // counter to the first array
           k <- m         // counter for the second array
           para i de first_idx ate last_idx faca
                se j >= m entao
                   a[i] <- t[k]
                   k <- k + 1
                senao
                     se k > last_idx entao
                     a[i] <- t[j]
                      j <- j + 1
                     senao
                          se t[k] < t[j] entao
                          a[i] <- t[k]
                          k <- k + 1
                          senao
                               a[i] <- t[j]
                               j <- j + 1
                          fimse
                     fimse
                fimse
           fimpara
        fimse
fimprocedimento


inicio
        para i de 1 ate 8 faca
             a[i] <- 8 - i
             t[i] <- a[i]
        fimpara
        put_a()
        msort(1, 8)
        put_a()
fimalgoritmo
