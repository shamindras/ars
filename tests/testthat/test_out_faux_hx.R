context("test_out_faux_hx: Check Output of faux_hx")

test_that("Outputs are Validated", {
  
  # Test 1 - Check that the output is a function
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  y_test <- faux_hx(inp_gfun = g)
  expect_equal(class(y_test), "function")
  
  # Test 2 - check that the output is contains only one element 
  g <- function(x) x^2 # valid function
  y_test <- faux_hx(inp_gfun = g)
  expect_equal(length(y_test),1)
  
  # Test 3 - check that the output gives the log(g(x))
  g <- function(x) x^2 # valid function
  y_test <- faux_hx(inp_gfun = g)
  expect_equal(y_test(4),log(4^2))
  
})