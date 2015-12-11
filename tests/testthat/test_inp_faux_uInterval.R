context("faux_uInterval: Check Inputs")

test_that("Inputs are Valid", {
  
  # Test 1
  zvals <- list(1,2,3) # invalid format, must be a vector
  expect_error(faux_uInterval(zvals))
  
  # Test 2
  zvals <- c(1,2,5,3) # invalid vector of inputs, not in increasing order
  expect_error(faux_uInterval(zvals))
  

})