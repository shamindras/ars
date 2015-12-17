context("test-out_faux_hx: Check Output of faux_hx")

test_that("test-out_faux_hx: Outputs are Validated", {
  
  # Test 1 - Check that the output is a function
  set.seed(0)
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  y_test <- faux_hx(inp_gfun = g)
  expect_equal(class(y_test), "function")
  
  # Test 2 - check that the output is contains only one element 
  set.seed(0)
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  y_test <- faux_hx(inp_gfun = g)
  expect_equal(length(y_test),1)
  
  # Test 3 - check that the output gives the log(g(x))
  set.seed(0)
  g <-  function(x) 2*exp(-2*x)  # valid function
  y_test <- faux_hx(inp_gfun = g)
  expect_equal(y_test(100),log(2)-2*100)
  expect_equal(y_test(-200),log(2)+2*200)
  # expect_equal(y_test(10000),log(2)-2*10000)
  
})