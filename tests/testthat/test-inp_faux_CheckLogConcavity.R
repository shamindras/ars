context("test-inp_faux_CheckLogConcavity: Check input validity of faux_CheckLogConcavity")

test_that("test-inp_faux_CheckLogConcavity: Inputs are validated", {
  
  # Test 1
  set.seed(0)
  gfun <- "x^2"           # invalid function
  Dvec <- c(1,3)          # valid vector
  # Expected error
  expect_error(faux_CheckLogConcavity(gfun,Dvec))
  
  # Test 2
  set.seed(0)
  gfun <- function(x) 8*exp(-8*x) # valid function
  Dvec <- c("a","b")              # invalid vector; should be numeric
  # Expected error
  expect_error(faux_CheckLogConcavity(gfun,Dvec))
  
  # Test 3
  set.seed(0)
  gfun <- function(x) 8*exp(-8*x) # valid function
  Dvec <- c(1,2,3)                # invalid vector; should have 2 elements
  # Expected error
  expect_error(faux_CheckLogConcavity(gfun,Dvec))

  # Test 4
  set.seed(0)
  gfun <- function(x) 8*exp(-8*x) # valid function
  Dvec <- c(8, 8)                 # invalid vector; should have unique elements
  # Expected error
  expect_error(faux_CheckLogConcavity(gfun,Dvec))
  
})
