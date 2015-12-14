context("test_out_faux_Lkx: Check Output of faux_Lkx")

test_that("test_out_faux_Lkx: Outputs are Validated", {
  
  # Test 1 - Check that the output is a list 
  set.seed(0)
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(3.5, 6.7)  #valid x value 
  y_test <- faux_Lkx(inp_xvec = Xvec, inp_gfun = g)
  expect_equal(class(y_test), "list")
  
  # Test 2 - check that the elements of the output are functions
  set.seed(0)
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-10.5, 8.9, 111.6, 353.0)  #valid x value 
  y_test <- faux_Lkx(inp_xvec = Xvec, inp_gfun = g)
  expect_equal(sapply(y_test,class),
               rep("function",length(y_test)))
  
  # Test 3 - check that the length of the list should be length(Xvec)+1
  set.seed(0)
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-10.5, 5)  #valid x value 
  y_test <- faux_Lkx(inp_xvec = Xvec, inp_gfun = g)
  expect_equal(length(y_test), length(Xvec)+1)
  
  # Test 4 - Test that the first element and last element in the list always 
  # return a function with constant value def-faux-Lkx-negInf na matter what 
  #value is passed in 
  set.seed(0)
  g <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function
  Xvec <- c(-10.7,3.5,20.6)       #valid vector of x 
  y_test <- faux_Lkx(inp_xvec = Xvec, inp_gfun = g)
  #The evaluation of the first function in the list should be -Inf no matter 
  #what value we pass in to the function 
  expect_equal(y_test[[1]](10), def_faux_Lkx_negInf)
  expect_equal(y_test[[1]](-25),def_faux_Lkx_negInf)
  ##The evaluation of the last function in the list should be -Inf no matter 
  #what value we pass in to the function 
  expect_equal(y_test[[length(y_test)]](100000), def_faux_Lkx_negInf)
  expect_equal(y_test[[length(y_test)]](-200000053423),def_faux_Lkx_negInf)
  
  # Test 5 - Test that the function works for the values we known 
  set.seed(0)
  g <- function(x) {2*exp(-2*x)}        #Valid function 
  Xvec <- c(-5,-2,4,13,19)
  y_test <- faux_Lkx(inp_xvec = Xvec, inp_gfun = g)
  #If x* lies in between (-5,-2), x*=-3.8
  expect_equal(y_test[[2]](-3.8),(1.8*(log(2)+10)+1.2*(log(2)+4))/3)
  #If x* lies in between (-2,4), x*=0 
  expect_equal(y_test[[3]](0),(4*(log(2)+4)+2*(log(2)-8))/6)
  #If x* lies in between (4,13), x*=7.9
  expect_equal(y_test[[4]](7.9),(5.1*(log(2)-8)+3.9*(log(2)-26))/9)
  #If x* lies in between (13,19), x*= 17.125
  expect_equal(y_test[[5]](17.125),(1.875*(log(2)-26)+4.125*(log(2)-38))/6)
  #If x*<-5, it will return def_faux_Lkx_negInf 
  expect_equal(y_test[[1]](-200003),def_faux_Lkx_negInf)
  #If x*>19, it will return def_faux_Lkx_negInf 
  expect_equal(y_test[[length(y_test)]](82.35789124),def_faux_Lkx_negInf)
})

