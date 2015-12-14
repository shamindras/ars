context("test-out_faux_hPrimex: Check Output of faux_hPrimex")

test_that("test-out_faux_hPrimex: Outputs are Validated", {
  
  # Test 1 - Check that the output is a numeric vector 
  set.seed(0)
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- 3  #valid x value (contains only one element)
  y_test <- faux_hPrimex(inp_gfun = g, inp_xvec = Xvec)
  expect_equal(class(y_test), "numeric")
  
  # Test 2 - Check that the output is a numeric vector 
  set.seed(0)
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(10,20)  #valid x value (contains more than one element)
  y_test <- faux_hPrimex(inp_gfun = g, inp_xvec = Xvec)
  expect_equal(class(y_test), "numeric")
  
  # Test 3 - check that each element of the output is numeric 
  set.seed(0)
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-38,5,20,30)  # valid x value 
  y_test <- faux_hPrimex(inp_gfun = g, inp_xvec = Xvec)
  expect_equal(sapply(y_test,class),rep("numeric",length(Xvec)))
  
  # Test 4 - check that the length of the output is the same as the length of x
  set.seed(0)
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- 2  # valid x value (contains only one element)
  y_test <- faux_hPrimex(inp_gfun = g, inp_xvec = Xvec)
  expect_equal(length(y_test),length(Xvec))
  
  # Test 5 - check that the length of the output is the same as the length of x
  set.seed(0)
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(4.5,8.6,10.3)  # valid x value (contains more than one element) 
  y_test <- faux_hPrimex(inp_gfun = g, inp_xvec = Xvec)
  expect_equal(length(y_test),length(Xvec))
  
  #Test 6 - check that the output gives h'(x) evaluated at x 
  set.seed(0)
  g <-  function(x) 2*exp(-2*x) # valid function
  Xvec <- c(1:200)  # valid x value
  y_test <- faux_hPrimex(inp_gfun = g, inp_xvec = Xvec)
  expect_equal(y_test,rep(-2,length(Xvec)))
  
})