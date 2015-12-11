context("test_out_faux_Lkx: Check Output of faux_Lkx")

test_that("Outputs are Validated", {
  
  # Test 1 - Check that the output is a list 
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(3.5, 6.7)  #valid x value 
  y_test <- faux_Lkx(inp_xvec = Xvec, inp_gfun = g)
  expect_equal(class(y_test), "list")
  
  # Test 2 - check that the first element of the output is numeric
  # The first element should be -Inf 
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-10.5, 8.9, 111.6)  #valid x value 
  y_test <- faux_Lkx(inp_xvec = Xvec, inp_gfun = g)
  expect_equal(class(y_test[[1]]), "numeric")
  expect_equal(y_test[[1]],def_faux_Lkx_negInf)
  
  # Test 3 - check that the last element of the output is numeric
  # The last element should be -Inf 
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-10.5, 8.9, 111.6)  #valid x value 
  y_test <- faux_Lkx(inp_xvec = Xvec, inp_gfun = g)
  expect_equal(class(y_test[[length(y_test)]]), "numeric")
  expect_equal(y_test[[length(y_test)]],def_faux_Lkx_negInf)
  
  # Test 4 - check that the 2:k elements of the output is function
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-10.5, 8.9, 111.6, 353.0)  #valid x value 
  y_test <- faux_Lkx(inp_xvec = Xvec, inp_gfun = g)
  #get rid the of the first element and last element 
  expect_equal(sapply(y_test[c(2:(length(y_test)-1))],class),
               rep("function",length(y_test)-2))
  
  # Test 5 - check that the length of the list should be length(Xvec)+1
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-10.5, 5)  #valid x value 
  y_test <- faux_Lkx(inp_xvec = Xvec, inp_gfun = g)
  expect_equal(length(y_test), length(Xvec)+1)
  
})

