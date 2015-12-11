context("test_inp_faux_hx: Check Input Validity of faux_hx")

test_that("Inputs are Validated", {
  
  # Test 1
  g <- "x^10"           # invalid function 
  # Expected error
  expect_error(faux_hx(inp_gfun = g))
  
  # Test 2
  g <- 9                # invalid function
  #Expected error 
  expect_error(faux_hx(inp_gfun = g))
  
})