context("test_inp_faux_CheckLogConcavity: Check input validity of faux_CheckLogConcavity")

test_that("test_inp_faux_CheckLogConcavity: Inputs are validated", {
  
  # Test 1
  xvec <- c(1,2,3)        # valid vector
  gfun <- "x^2"           # invalid function
  # Expected error
  expect_error(faux_CheckLogConcavity(xvec,gfun))
  
  # Test 2
  xvec <- c("a","b","c")  # invalid vector; should be numeric
  gfun <- function(x) 8*exp(-8*x) # valid function
  # Expected error
  expect_error(faux_CheckLogConcavity(xvec,gfun))
  
  # Test 3
  xvec <- c(1,2)          # invalid vector; should have 3 elements
  gfun <- function(x) 8*exp(-8*x) # valid function
  # Expected error
  expect_error(faux_CheckLogConcavity(xvec,gfun))

  # Test 4
  xvec <- c(8, 8, 8)          # invalid vector; should have 3 elements
  gfun <- function(x) 8*exp(-8*x) # valid function
  # Expected error
  expect_error(faux_CheckLogConcavity(xvec,gfun))
  
})
