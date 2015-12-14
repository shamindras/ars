context("test-out_InitChoose_HPrime: Check Output of InitChoose and HPrime")

test_that("test-out_InitChoose_HPrime: Outputs are Validated", { 
  
  # Test 1 - check that when the initial points are generated, hPrime(x) does 
  # not produce NA values 
  # Initial points lies in between (-30, 30)
  set.seed(0)
  # Normal distribution in explicit form 
  inp_gfun <- function(x) {(2*pi)^(-0.5)*exp(-0.5*(x)^2)}     #valid function
  inp_Dvec <- c(-Inf,Inf)                                      #valid input 
  y_test <- faux_InitChoose(inp_gfun,inp_Dvec,200000)$init_sample_points
  #For standar normal distribution,h(x)=-0.5log(2pi)-0.5x^2 
  #h'(x)=-x
  expect_equal(faux_hPrimex(inp_gfun,y_test),-y_test)
  
  # Test 2 - check that when the initial points are generated, hPrime(x) does 
  # not produce NA values 
  set.seed(0)
  inp_gfun <- dnorm                                         #valid function
  inp_Dvec <- c(-3,5)                                      #valid input 
  y_test <- faux_InitChoose(inp_gfun,inp_Dvec,6)$init_sample_points
  #For standar normal distribution,h(x)=-0.5log(2pi)-0.5x^2 
  #h'(x)=-x
  expect_equal(faux_hPrimex(inp_gfun,y_test),-y_test)
  
  # Test 3 - check that when the initial points are generated, hPrime(x) does 
  # not produce NA values 
  set.seed(0)
  inp_gfun <- function(x) dchisq(x, df = 5)                  #valid function
  inp_Dvec <- c(0, Inf)                                      #valid input 
  y_test <- faux_InitChoose(inp_gfun,inp_Dvec,6)$init_sample_points
  mode <- faux_InitChoose(inp_gfun, inp_Dvec,6)$mode
  #For chisq(x,df=5), mode=3 
  expect_equal(mode,3)
  #h'(x) > 0 if x < mode 
  #h'(x) < 0 if x > mode 
  expect_true(all(faux_hPrimex(inp_gfun,y_test[y_test<mode]) > 0))
  expect_true(all(faux_hPrimex(inp_gfun,y_test[y_test>mode]) < 0))
  #6 initial points are sampled 
  expect_equal(length(y_test),6)
  
  # Test 4 - check that when the initial points are generated, hPrime(x) does 
  # not produce NA values 
  # Chi-square distribution in explicit form chisq(df=10)
  set.seed(0)
  inp_gfun <- function(x) 1/(2^(10/2)*gamma(10/2))*x^(4)*exp(-x/2) #valid function
  inp_Dvec <- c(0, Inf)                                      #valid input 
  y_test <- faux_InitChoose(inp_gfun,inp_Dvec,10)$init_sample_points
  mode <- faux_InitChoose(inp_gfun, inp_Dvec)$mode
  #For chisq(x,df=10), mode=8
  expect_equal(mode,8)
  #h'(x) > 0 if x < mode 
  #h'(x) < 0 if x > mode 
  expect_true(all(faux_hPrimex(inp_gfun,y_test[y_test<mode]) > 0))
  expect_true(all(faux_hPrimex(inp_gfun,y_test[y_test>mode]) < 0))
  #10 initial points are sampled 
  expect_equal(length(y_test),10)
  #Acutal value of h'(x) calculated by hand 
  hPrime <- function(x) 4/x-1/2
  hPrime_vec <- hPrime(y_test)
  expect_true(identical(all.equal(faux_hPrimex(inp_gfun,y_test),
                                  hPrime_vec),TRUE))
  
  # Test 5 - check that when the initial points are generated, hPrime(x) does 
  # not produce NA values 
  # exponential distribution with lambda=5.5 
  inp_gfun <- function(x) dexp(x, rate = 5.5)                  #valid function
  inp_Dvec <- c(0, Inf)                                        #valid input 
  y_test <- faux_InitChoose(inp_gfun,inp_Dvec,200)$init_sample_points
  mode <- faux_InitChoose(inp_gfun, inp_Dvec,200)$mode
  #For exponential distribution, mode = 0 
  expect_equal(mode,0)
  #h'(x) < 0 if x > mode 
  #no x < mode since Domain is (0,Inf), mode = 0 
  expect_true(all(faux_hPrimex(inp_gfun,y_test[y_test>mode]) < 0))
  #The value of h'(x) should be -5.5 (calculated by hand)
  expect_true(identical(all.equal(faux_hPrimex(inp_gfun,y_test),
                                   rep(-5.5,length(y_test))),TRUE))
  
  # Test 6 - check that when the initial points are generated, hPrime(x) does 
  # not produce NA values 
  # exponential distribution with lambda=8 in explicit form 
  inp_gfun <- function(x) 8*exp(-8*x)                 #valid function
  inp_Dvec <- c(0, Inf)                                   #valid input
  y_test <- faux_InitChoose(inp_gfun,inp_Dvec,100)$init_sample_points
  mode <- faux_InitChoose(inp_gfun, inp_Dvec,100)$mode
  #For exponential distribution, mode = 0 
  expect_equal(mode,0)
  #h'(x) < 0 if x > mode 
  #no x < mode since Domain is (0,Inf), mode = 0 
  expect_true(all(faux_hPrimex(inp_gfun,y_test[y_test>mode]) < 0))
  #The value of h'(x) should be -5.5 (calculated by hand)
  expect_true(identical(all.equal(faux_hPrimex(inp_gfun,y_test),
                                  rep(-8,length(y_test))),TRUE))
  
  # Test 7 - check that when the initial points are generated, hPrime(x) does 
  # not produce NA values 
  # beta distribution with shape1=2 and shape2=5
  inp_gfun <- function(x) dbeta(x, shape1 = 2, shape2 = 5)     #valid function
  inp_Dvec <- c(0, 1)                                        #valid input 
  y_test <- faux_InitChoose(inp_gfun,inp_Dvec,20)$init_sample_points
  mode <- faux_InitChoose(inp_gfun, inp_Dvec,20)$mode
  #For beta distribution, mode = 4
  expect_equal(round(mode,4),0.2)
  expect_true(all(faux_hPrimex(inp_gfun,y_test[y_test<mode]) > 0))
  #h'(x) < 0 if x > mode 
  expect_true(all(faux_hPrimex(inp_gfun,y_test[y_test>mode]) < 0))
  #The value of h'(x) should be the following vector (calculated by hand)
  hPrime <- function(x) 1/x-4/(1-x)
  hPrime_vec <- hPrime(y_test)
  expect_true(identical(all.equal(faux_hPrimex(inp_gfun,y_test),
                                  hPrime_vec),TRUE))
  
  # Test 8 - check that when the initial points are generated, hPrime(x) does 
  # not produce NA values 
  # beta distribution with shape1=2 and shape2=5 in explicit form 
  inp_gfun <- function(x) {
     gamma(2+5)/(gamma(2)*gamma(5))*x^1*(1-x)^4 
  }                                                          #valid function
  inp_Dvec <- c(0, 1)                                        #valid input 
  y_test <- faux_InitChoose(inp_gfun,inp_Dvec,20)$init_sample_points
  mode <- faux_InitChoose(inp_gfun, inp_Dvec,20)$mode
  #For beta distribution, mode = 4
  expect_equal(round(mode,4),0.2)
  expect_true(all(faux_hPrimex(inp_gfun,y_test[y_test<mode]) > 0))
  #h'(x) < 0 if x > mode 
  expect_true(all(faux_hPrimex(inp_gfun,y_test[y_test>mode]) < 0))
  #The value of h'(x) should be the following vector (calculated by hand)
  hPrime <- function(x) 1/x-4/(1-x)
  hPrime_vec <- hPrime(y_test)
  expect_true(identical(all.equal(faux_hPrimex(inp_gfun,y_test),
                                  hPrime_vec),TRUE))
})
                                   
  
  