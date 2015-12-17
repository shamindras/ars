context("test-inp_faux_hx: Check Input Validity of faux_hx")

test_that("test-inp_faux_hx: Inputs are Validated", {
  
  # Test 1
  set.seed(0)
  g <- "x^10"           # invalid function 
  # Expected error
  expect_error(faux_hx(inp_gfun = g))
  
  # Test 2
  set.seed(0)
  g <- 9                # invalid function
  #Expected error 
  expect_error(faux_hx(inp_gfun = g))

  # Test 3
  set.seed(0)
  g <- function(x) 2*exp(-2*x)                # valid function
  h <- faux_hx(inp_gfun = g) 
  expect_that(h(1), equals(log(2) - 2))       # Test against know value

  # Test 4
  set.seed(0)
  g <- function(x) dnorm(x)                # valid function
  h <- faux_hx(inp_gfun = g) 
  expect_that(h(0), equals(log(1/sqrt(2*pi))))       # Test against know value
  
})