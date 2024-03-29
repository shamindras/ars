context("test-inp_faux_SampleSkx: Check Inputs")

test_that("test-inp_faux_SampleSkx: Inputs are Validated", {
  
  # Test 1 
  set.seed(0)
  uint <- c(c(-1,0),c(0,1)) # invalid format for intervals
  inp_sfunlist <- faux_Skx(list(c(-1,0),c(0,1)),
      list(function(x) x+3, function(x) 3-x)) # valid input of skx function
  # Expected error
  expect_error(faux_SampleSkx(uint, inp_sfunlist))
  
#   # Test 2
#   set.seed(0)
#   uint <- list(c(-1,0),c(0,1)) # valid format for intervals
#   inp_sfunlist <- data.frame(function(x) x+3, function(x) 3-x) # invalid format for 
#   # inp_sfunlist, needs to be a list
#   # Expected error
#   expect_error(faux_SampleSkx(uint, inp_sfunlist))
  
  # Test 3
  set.seed(0)
  uint <- c(c(1,2,5),c(2,3)) # invalid length of intervals
  inp_sfunlist <- faux_Skx(list(c(-1,0),c(0,1)),
          list(function(x) x+3, function(x) 3-x)) # valid input of skx function
  # Expected error
  expect_error(faux_SampleSkx(uint, inp_sfunlist))
  
  # Test 4
  set.seed(0)
  uint <- list(c(1,2),c(2,3)) # valid list of intervals
  inp_sfunlist <- list(3, function(x) 3*x+.5) # invalid element of the list, must be a
  # function
  # Expected error
  expect_error(faux_SampleSkx(uint, inp_sfunlist))
  
  # Test 5
  set.seed(0)
  uint <- list(c(1,2),c(2,3), c(3,4)) # valid list of intervals, length 3
  inp_sfunlist <- list(function(x) x+1, function(x) 3*x+.5) 
  # valid list of functions, length 2
  # Expected error
  expect_error(faux_Skx(uint, inp_sfunlist))
  
})