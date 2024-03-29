context("test-inp_faux_uInterval: Check Inputs")

test_that("test-inp_faux_uInterval: Inputs are Validated", {
  
  # Test 1
  set.seed(0)
  zvals <- list(1,2,3) # invalid format, must be a vector
  expect_error(faux_uInterval(zvals))
  
  # Test 2
  set.seed(0)
  zvals <- list(Inf,1,2,3,-Inf) # invalid format, must be a vector
  expect_error(faux_uInterval(zvals))

  # Test 3
  set.seed(0)
  zvals <- data.frame(Inf,1,2,3,0,4,-Inf) # invalid format, must be a vector
  expect_error(faux_uInterval(zvals))
  
  # Test 4
  set.seed(0)
  zvals <- c(Inf,1,2,3,0,4,-Inf) # valid format for inputs
  # check that gven this set of inputs you get the right list of intervals
  expect_that(faux_uInterval(zvals),equals(list(c(-Inf,0),c(0,1),c(1,2),c(2,3)
                                                ,c(3,4),c(4,Inf))))
  
})