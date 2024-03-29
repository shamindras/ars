context("test-inp_faux_Skx: Check Inputs")

test_that("test-inp_faux_Skx: Inputs are Validated", {
  
  # Test 1
  set.seed(0)
  uint <- list(c(1,2),c(2,3)) # valid list of intervals
  ufuns <- function(x) x^2 # invalid input, needs to be a list of functions
  # Expected error
  expect_error(faux_Skx(uint, ufuns))
  
  # Test 2
  set.seed(0)
  uint <- c(c(1,2),c(2,3)) # invalid vector of intervals, not a list
  ufuns <- list(function(x) x+1, function(x) 3*x+.5) # valid input for list of
  # functions
  # Expected error
  expect_error(faux_Skx(uint, ufuns))
  
  # Test 3
  set.seed(0)
  uint <- c(c(1,2,5),c(2,3)) # invalid length of intervals
  ufuns <- list(function(x) x+1, function(x) 3*x+.5) # valid input for list of
  # functions
  # Expected error
  expect_error(faux_Skx(uint, ufuns))
  
  # Test 4
  set.seed(0)
  uint <- list(c(1,2),c(2,3)) # valid list of intervals
  ufuns <- list(3, function(x) 3*x+.5) # invalid element of the list, must be a
  # function
  # Expected error
  expect_error(faux_Skx(uint, ufuns))
  
  # Test 5
  set.seed(0)
  uint <- list(c(1,2),c(2,3), c(3,4)) # # valid list of intervals, length 3
  ufuns <- list(function(x) x+1, function(x) 3*x+.5) 
  # valid list of functions, length 2
  # Expected error
  expect_error(faux_Skx(uint, ufuns))
  
  })