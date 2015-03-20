algoritmo "Potência 2"
var k, p, q  : real
    n, i : inteiro
    j, m: inteiro
inicio
   repita
     escreval("Entre com um real k, e um inteiro n >= 0: ")
     leia(k, n)
   ate n >= 0

   se n = 0 entao
     escreval(1)
   senao
     m <- n
     j <- -1
     repita
       j <- j + 1
       q <- raizq(m - j)
     ate (int(q) = q) ou (m = j)
     escreva("Pot quad mais proxima de ", n,
               " e ", m - j, " , dif = ", j)

     p <- k
     i <- m - j
     enquanto i > 1 faca
        p <- p * p
        i <- i \ 2
     fimenquanto
     i <- 0
     enquanto i < j faca
        p <- k * p
        i <- i + 1
     fimenquanto
     escreval(k, "^", n, " = ", p)
   fimse
fimalgoritmo
