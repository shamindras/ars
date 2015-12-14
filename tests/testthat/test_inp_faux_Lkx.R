context("test_inp_faux_Lkx: Check Input Validity of faux_Lkx")

test_that("test_inp_faux_Lkx: Inputs are Validated", {
  
  # Test 1
  set.seed(0)
  g    <- "x^2"           # invalid function 
  Xvec <- c(3,5)          # valid vector x 
  # Expected error
  expect_error(faux_Lkx(inp_xvec = Xvec, inp_gfun = g))

  # Test 2
  set.seed(0)
  g    <- "x^2"           # invalid function 
  Xvec <- c(5,10,"a")     # invalid vector x
  # Expected error
  expect_error(faux_Lkx(inp_xvec = Xvec, inp_gfun = g))
  
  # Test 3
  set.seed(0)
  g    <- dnorm           # valid function 
  Xvec <- 3               # invalid vector x 
  # Expected error
  expect_error(faux_Lkx(inp_xvec = Xvec, inp_gfun = g))
  
  #Test 4
  set.seed(0)
  g    <- function(x) 11*exp(-11*x) #valid function 
  Xvec <- "a"                       #invalid vector
  # Expected error
  expect_error(faux_Lkx(inp_xvec = Xvec, inp_gfun = g))
  
})