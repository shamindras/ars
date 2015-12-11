context("test_inp_faux_Lkx: Check Input Validity of faux_Lkx")

test_that("Inputs are Validated", {
  
  # Test 1
  g    <- "x^2"           # invalid function 
  Xvec <- c(3,5)          # valid vector x 
  # Expected error
  expect_error(faux_Lkx(inp_xvec = Xvec, inp_gfun = g))

  # Test 2
  g    <- "x^2"           # invalid function 
  Xvec <- c(5,10,"a")     # invalid vector x
  # Expected error
  expect_error(faux_Lkx(inp_xvec = Xvec, inp_gfun = g))
  
  # Test 3
  g    <- function(x) x^2 # valid function 
  Xvec <- 3               # invalid vector x 
  # Expected error
  expect_error(faux_Lkx(inp_xvec = Xvec, inp_gfun = g))
  
  # Test 4
  g    <- function(x) x^2 # valid function 
  Xvec <- c(9,3)          # invalid vector should be increasing order 
  # Expected error
  expect_error(faux_Lkx(inp_xvec = Xvec, inp_gfun = g))
  
  #Test 5 
  g    <- function(x) x^2 #valid function 
  Xvec <- c(8,5,9,10)     #invalid vector should be increasing order 
  # Expected error
  expect_error(faux_Lkx(inp_xvec = Xvec, inp_gfun = g))
  
  #Test 6
  g    <- function(x) x^2 #valid function 
  Xvec <- "a"             #invalid vector should be increasing order 
  # Expected error
  expect_error(faux_Lkx(inp_xvec = Xvec, inp_gfun = g))
  
})