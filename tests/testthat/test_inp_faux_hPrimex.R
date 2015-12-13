context("test_inp_faux_hPrimex: Check Input Validity of faux_hPrimex")

test_that("test_inp_faux_hPrimex: Inputs are Validated", {
  
  # Test 1
  g    <- function(x) x^2 # valid function 
  Xvec <- c()    # invalid vector should have at least one elements!
  # Expected error
  expect_error(faux_hPrimex(inp_gfun = g, inp_xvec = Xvec))
  
  # Test 2
  g    <- "x^2"           # invalid function 
  Xvec <- c(5,20)       # valid vector should only have 2 elements!
  # Expected error
  expect_error(faux_hPrimex(inp_gfun = g, inp_xvec = Xvec))
  
  # Test 3
  g    <- function(x) x^2 # valid function 
  Xvec <- c(10, "a")      # invalid vector - must be numeric
  # Expected error
  expect_error(faux_hPrimex(inp_gfun = g, inp_xvec = Xvec))
  
  # Test 4
  g    <- "x^2"           # invalid function 
  Xvec <- c("a")      # invalid vector - must be numeric
  # Expected error
  expect_error(faux_hPrimex(inp_gfun = g, inp_xvec = Xvec))
  
  # Test 5
  g    <- function(x) x^2 # valid function 
  Xvec <- "b"      # invalid vector - must be numeric 
  # Expected error
  expect_error(faux_hPrimex(inp_gfun = g, inp_xvec = Xvec))
})