context("test_out_faux_Zj: Check Output of faux_Zj")

test_that("test_out_faux_Zj: Outputs are Validated", {
  
  # Test 1 - Check that the output is a numeric vector 
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(3.5, 6.7)  #valid x value 
  Dvec <- c(-Inf, Inf)                               # valid Support
  y_test <- faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec)
  expect_equal(class(y_test), "numeric")
  
  # Test 2 - check that each element of the output is numeric
  # If we have k x values, we should have k+1 z values 
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-7, 8.9)  #valid x value 
  Dvec <- c(-20, 10)                               # valid Support
  y_test <- faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec)
  expect_equal(sapply(y_test,class),rep("numeric",length(Xvec)+1))
  
  # Test 3 - check that the length of the output is the same as the length of x
  # plus 1
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-5,10.7)  # valid x value (2 elements)
  Dvec <- c(-Inf, Inf) # valid Support
  y_test <- faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec)
  expect_equal(length(y_test),length(Xvec)+1)
  
  # Test 4 - check that the length of the output is the same as the length of x
  # plus 1
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-5,10.7,11.8,30.85)  # valid x value 
  Dvec <- c(-Inf, Inf) # valid Support
  y_test <- faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec)
  expect_equal(length(y_test),length(Xvec)+1)
  
  # Test 5 - check that the first element of the output is the same as the 
  #first element of Dvec 
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-5,10.7,11.8,30.85)  # valid x value 
  Dvec <- c(-Inf, Inf) # valid Support
  y_test <- faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec)
  expect_equal(y_test[1],Dvec[1])
  
  # Test 6 - check that the last element of the output is the same as the 
  #first element of Dvec 
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-5,10.7,11.8,30.85)  # valid x value 
  Dvec <- c(-Inf, 400) # valid Support
  y_test <- faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec)
  expect_equal(y_test[length(y_test)],Dvec[2])
  
  #Test 7 - check that the output gives correct values of Zj when h'(x) is 
  #the same everyhwere. We just take the Zj's to be midpoints of Xj and Xj+1
  g <- function(x) 2*exp(-2*x) # valid function
  Xvec <- c(3,5,8,15)  # valid x value 
  Dvec <- c(-10,20) # valid Support
  y_test <- faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec)
  Truevec <- c(-10,4,6.5,11.5,20)
  expect_equal(y_test,Truevec)
  
  #Test 8 -Check that the output gives correct values of Zj when h'(x) is 
  #different. For example, z[1]=D[1], z[2] in the interval(x[1],x[2]), 
  #z[3] in the interval (x[2],x[3]),z[4] in the interval (x[3],x[4]) and
  #z[5]=D[2]
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-5,10.7,11.8,30.85)  # valid x value 
  Dvec <- c(-Inf, 400) # valid Support
  y_test <- faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec)
  expect_equal(y_test[1],Dvec[1])
  expect_true(y_test[2]<=Xvec[2]&&y_test[2]>=Xvec[1])
  expect_true(y_test[3]<=Xvec[3]&&y_test[3]>=Xvec[2])
  expect_true(y_test[4]<=Xvec[4]&&y_test[4]>=Xvec[3])
  expect_equal(y_test[5],Dvec[2])
})

