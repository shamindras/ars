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
  Xvec <- c(-5,10.7,11.8,308.5)  # valid x value 
  Dvec <- c(-Inf, Inf) # valid Support
  y_test <- faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec)
  expect_equal(length(y_test),length(Xvec)+1)
  
  # Test 5 - check that the first element of the output is the same as the 
  #first element of Dvec 
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-5,10.7,11.8,308.5)  # valid x value 
  Dvec <- c(-Inf, Inf) # valid Support
  y_test <- faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec)
  expect_equal(y_test[1],Dvec[1])
  
  # Test 6 - check that the last element of the output is the same as the 
  #first element of Dvec 
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-5,10.7,11.8,308.5)  # valid x value 
  Dvec <- c(-Inf, 400) # valid Support
  y_test <- faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec)
  expect_equal(y_test[length(y_test)],Dvec[2])
  
  #Test 7 - check that the output gives correct values of Zj
  g <- function(x) x^2 # valid function
  Xvec <- c(3,5,8,15)  # valid x value 
  Dvec <- c(-10,20) # valid Support
  y_test <- faux_Zj(inp_xvec = Xvec, inp_gfun = g, inp_Dvec = Dvec)
  Truevec <- c()
  Truevec[1] <- Dvec[1]
  Truevec[length(Xvec)+1]  <- Dvec[2]
  h_x <- faux_hx(g)
  Hvec <- h_x(Xvec)
  HPrimevec <- faux_hPrimex(g, Xvec)
  Truevec[2] <- (h_x(Xvec[2])-h_x(Xvec[1])-Xvec[2]*HPrimevec[2]+Xvec[1]*HPrimevec[1])/
                   (HPrimevec[1]-HPrimevec[2])
  Truevec[3] <- (h_x(Xvec[3])-h_x(Xvec[2])-Xvec[3]*HPrimevec[3]+Xvec[2]*HPrimevec[2])/
                   (HPrimevec[2]-HPrimevec[3])
  Truevec[4] <- (h_x(Xvec[4])-h_x(Xvec[3])-Xvec[4]*HPrimevec[4]+Xvec[3]*HPrimevec[3])/
                   (HPrimevec[3]-HPrimevec[4])
  expect_equal(y_test,Truevec)
  
})

