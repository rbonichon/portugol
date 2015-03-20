algoritmo "heapsort"
    var a, t : vetor [1..8] de inteiro
        i:inteiro
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

procedimento heapify(fidx, lidx: inteiro)
var sidx: inteiro
inicio
        sidx <- int((lidx - fidx) / 2.0)
        enquanto sidx > 0 faca
                 siftdown(sidx, lidx)
                 sidx <- sidx - 1
        fimenquanto
fimprocedimento

procedimento swap(i, j: inteiro)
var tmp: inteiro
inicio
        tmp <- a[i]
        a[i] <- a[j]
        a[j] <- tmp
fimprocedimento

procedimento hsort(fidx, lidx: inteiro)
var end:inteiro
inicio
        heapify(fidx, lidx)
        end <- lidx
        enquanto end > 0 faca
                 swap(a[end], a[0])
                 end <- end - 1
                 siftdown(0, end)
        fimenquanto
fimprocedimento

procedimento siftdown(fidx, lidx:inteiro)
var root, swidx, child: inteiro
    continue: logico
inicio
        continue <- verdadeiro
        root <- fidx
        enquanto continue e (root * 2 <= lidx) faca
                 child <- root * 2         // left child
                 swidx <- root              // keeps track of child to swap with
                 se a[swidx] < a[child] entao
                    swidx <- child
                 fimse
                 se (child + 1 <= lidx) e (a[swidx] < a[child + 1]) entao
                    swidx <- child + 1
                 fimse
                 se swidx <> root entao
                    swap(root, swidx)
                    root <- swidx
                 senao
                    continue <- falso
                 fimse
        fimenquanto
fimprocedimento

inicio
        para i de 1 ate 8 faca
             a[i] <- 8 - i
             t[i] <- a[i]
        fimpara
        put_a()
        hsort(1,8)
        put_a()

fimalgoritmo
