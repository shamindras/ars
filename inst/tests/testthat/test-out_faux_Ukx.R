context("test-out_faux_Ukx: Check Output of faux_Ukx")

test_that("test-out_faux_Ukx: Outputs are Validated", {
  
  # Test 1 - Check that the output is a list 
  set.seed(0)
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(3.5, 6.7)  #valid x value 
  y_test <- faux_Ukx(inp_xvec = Xvec, inp_gfun = g)
  expect_equal(class(y_test), "list")
  
  # Test 2 - check that the elements of the output are functions
  set.seed(0)
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-10.5, 8.9, 25, 22)  #valid x value 
  y_test <- faux_Ukx(inp_xvec = Xvec, inp_gfun = g)
  expect_equal(sapply(y_test,class),
               rep("function",length(y_test)))
  
  # Test 3 - check that the length of the list should be length(Xvec)
  set.seed(0)
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-10.5, 5)  #valid x value 
  y_test <- faux_Ukx(inp_xvec = Xvec, inp_gfun = g)
  expect_equal(length(y_test), length(Xvec))
  
})

