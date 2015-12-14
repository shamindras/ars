context("test_inp_faux_Zj: Check Input Validity of faux_Zj")

test_that("test_inp_faux_Zj: Inputs are Validated", {
  
  # Test 1
  set.seed(0)
  Xvec <- c(2,5,9,14)     # valid vector x 
  g    <- dnorm # valid function 
  Dvec <- c(10, 11, 3)    # invalid vector should only have 2 elements!
  # Expected error
  expect_error(faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec))
  
  # Test 2
  set.seed(0)
  g    <- "x^2"           # invalid function 
  Dvec <- c(10, 11)       # valid vector should only have 2 elements!
  Xvec <- c(3,5)          # valid vector x 
  # Expected error
  expect_error(faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec))
  
  # Test 3
  set.seed(0)
  g    <- function(x) 8*exp(-8*x) # valid function 
  Dvec <- c(10, "a")      # invalid vector - must be numeric
  Xvec <- c(9,40)         # valid vector x 
  # Expected error
  expect_error(faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec))
  
  # Test 4
  set.seed(0)
  g    <- "x^2"           # invalid function 
  Dvec <- c(10, "a")      # invalid vector - must be numeric
  Xvec <- c(5,10,"a")     # invalid vector x
  # Expected error
  expect_error(faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec))
  
  # Test 5
  set.seed(0)
  g    <- function(x) 8*exp(-8*x) # valid function 
  Dvec <- c(11, 10)       # invalid vector first element larger than second
  Xvec <- 3               # invalid vector x 
  # Expected error
  expect_error(faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec))
  
  # Test 6
  set.seed(0)
  g    <- function(x) 8*exp(-8*x) # valid function 
  Dvec <- c(11, 11)       # invalid vector first element equal to the second
  Xvec <- c(9,3)          # invalid vector should be increasing order 
  # Expected error
  expect_error(faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec))
  
  #Test 7 
  set.seed(0)
  g    <- function(x) 8*exp(-8*x) #valid function 
  Dvec <- c(-5,5)         #valid vector for domain 
  Xvec <- c(8,5,9,10)     #invalid vector should be increasing order 
  # Expected error
  expect_error(faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec))
  
  #Test 8 
  set.seed(0)
  g    <- dnorm           #valid function 
  Dvec <- c(-5,5)         #valid vector for domain 
  Xvec <- "a"             #invalid vector should be increasing order 
  # Expected error
  expect_error(faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec))
  
})