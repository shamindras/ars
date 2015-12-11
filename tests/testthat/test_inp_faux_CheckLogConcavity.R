context("test_inp_faux_CheckLogConcavity: Check input validity of faux_CheckLogConcavity")

test_that("Inputs are validated", {
  
  # Test 1
  xvec <- c(1,2,3)        # valid vector
  gfun <- "x^2"           # invalid function
  # Expected error
  expect_error(faux_CheckLogConcavity(xvec,gfun))
  
  # Test 2
  xvec <- c("a","b","c")  # invalid vector; should be numeric
  gfun <- function(x) x^2 # valid function
  # Expected error
  expect_error(faux_CheckLogConcavity(xvec,gfun))
  
  # Test 3
  xvec <- c(1,2)          # invalid vector; should have 3 elements
  gfun <- function(x) x^2 # valid function
  # Expected error
  expect_error(faux_CheckLogConcavity(xvec,gfun))
  
  # Test 4
  xvec <- c(3,2,1)        # invalid vector; should be in ascending order
  gfun <- function(x) x^2 # valid function
  # Expected error
  expect_error(faux_CheckLogConcavity(xvec,gfun))
  
})
