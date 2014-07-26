algoritmo "SEPA"
var len, key, newkey, tmp, i : inteiro
    v: vetor [1..10] de inteiro
inicio
    len <- 10
    para i de 1 ate len faca
         v[i] <- i - 1
    fimpara
    para i de 1 ate 10 faca
         len <- 10
         key <- len
         newkey <- len
         enquanto (key > 1) e (v[key] <= v[key - 1]) faca
                  key <- key - 1
         fimenquanto
         key <- key - 1
         se key >= 1 entao
            newkey <- 10
            enquanto (newkey > key) e (v[newkey] <= v[key]) faca
                     newkey <- newkey - 1
            fimenquanto
            tmp <- v[key]
            v[key] <- v[newkey]
            v[newkey] <- tmp
            len <- len - 1
            key <- key + 1
            enquanto (len > key) faca
                     tmp <- v[len]
                     v[len] <- v[key]
                     v[key] <- tmp
                     key <- key + 1
                     len <- len - 1
            fimenquanto
         fimse
         para i de 1 ate 10 faca
              escreva(v[i], " ")
         fimpara
         escreval(" ")
    fimpara
fimalgoritmo
