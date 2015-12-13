context("test_out_faux_Lkx: Check Output of faux_Lkx")

test_that("test_out_faux_Lkx: Outputs are Validated", {
  
  # Test 1 - Check that the output is a list 
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(3.5, 6.7)  #valid x value 
  y_test <- faux_Lkx(inp_xvec = Xvec, inp_gfun = g)
  expect_equal(class(y_test), "list")
  
  # Test 2 - check that the elements of the output are functions
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-10.5, 8.9, 111.6, 353.0)  #valid x value 
  y_test <- faux_Lkx(inp_xvec = Xvec, inp_gfun = g)
  #get rid the of the first element and last element 
  expect_equal(sapply(y_test,class),
               rep("function",length(y_test)))
  
  # Test 5 - check that the length of the list should be length(Xvec)+1
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-10.5, 5)  #valid x value 
  y_test <- faux_Lkx(inp_xvec = Xvec, inp_gfun = g)
  expect_equal(length(y_test), length(Xvec)+1)
  
})

