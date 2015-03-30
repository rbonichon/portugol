var ntests, failed: inteiro

procedimento test_init()
inicio
  ntests <- 0
  failed <- 0
fimprocedimento

procedimento pp_test(ex : logico)
inicio
  ntests <- ntests + 1
  se ex
  entao escreval("passed")
  senao
   failed <- failed + 1
   escreval("failed")
  fimse
fimprocedimento

procedimento itest_eq(e1, e2 : inteiro)
inicio
  escreva(e1," =? ", e2, "...")
  pp_test(e1 = e2)
fimprocedimento


procedimento rtest_eq(e1, e2 : real)
inicio
   escreva(e1," =? ", e2, "...")
   pp_test(e1 = e2)
fimprocedimento

procedimento ltest_eq(e1, e2: logico)
inicio
  escreva(e1," =? ", e2, "...")
  pp_test(e1 = e2)
fimprocedimento

procedimento test_end()
inicio
  escreval((ntests - failed) + " / " + ntests + " tests passed")
fimprocedimento
