algoritmo "Test primitives"
var ntests: inteiro
    success : inteiro

procedimento handle_test (val : logico)
inicio
  ntests <- ntests + 1
  se val entao
    escreva("success")
    success <- success + 1
  senao
    escreva("failed")
  fimse
fimprocedimento

funcao test_string(v1, v2: caractere) : inteiro
inicio
       escreval("Testing: ", v1, " = ", v2, "...")
       handle_test(v1 = v2)
fimfuncao

inicio
 ntests <- 0
 success <- 0
 test_string("foo", "foo!")
 test_string("foo", "foo")
 escreval("Tests: ", success, "/", ntests)
fimalgoritmo
