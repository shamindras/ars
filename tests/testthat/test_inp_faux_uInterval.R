context("test_inp_faux_uInterval: Check Inputs")

test_that("test_inp_faux_uInterval: Inputs are Valid", {
  
  # Test 1
  zvals <- list(1,2,3) # invalid format, must be a vector
  expect_error(faux_uInterval(zvals))

})