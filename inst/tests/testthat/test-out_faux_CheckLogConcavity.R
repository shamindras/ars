context("test-out_faux_CheckLogConcavity: Check validity of output")

test_that("test-out_faux_CheckLogConcavity: Outputs are Validated", {
  
  # Test 1 - Check that only 1 value is output
  set.seed(0)
  Dvec <- c(-Inf,Inf)                       # valid vector
  gfun <- dnorm                             # valid density
  out <- faux_CheckLogConcavity(gfun,Dvec)
  expect_equal(length(out), 1)
  
  # Test 2 - Check that value is Boolean
  set.seed(0)
  Dvec <- c(-Inf,Inf)                       # valid vector
  out <- faux_CheckLogConcavity(dnorm,Dvec) # valid density
  expect_equal(class(out),"logical")

  # Test 3 - Check that we pick out log CONVEX functions
  set.seed(0)
  Dvec <- c(2, 10)                   # valid vector
  inp_gfun <- function(x) exp(x^2)      # invalid function - log convex
  out <- faux_CheckLogConcavity(inp_gfun,Dvec) # valid density
  expect_false(out)

  # Test 4 - Check that we identify log CONCAVE functions
  set.seed(0)
  Dvec <- c(0,Inf)                   # valid vector
  inp_gfun <- function(x) 8*exp(-8*x)      # invalid function - log convex
  out <- faux_CheckLogConcavity(inp_gfun,Dvec)
  expect_true(out)

  # Test 5 - Check that we identify log CONCAVE functions
  set.seed(0)
  Dvec <- c(-Inf, Inf)                   # valid vector
  inp_gfun <- dnorm      # valid function - log convex
  out <- faux_CheckLogConcavity(inp_gfun,Dvec)
  expect_true(out)



})
