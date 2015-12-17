context("test-out_faux_Skx: Check output of Skx is a list of functions")

test_that("test-out_faux_Skx: Outputs are Validated", {
  
  # Test 1 - Check that the output is a list
  set.seed(0)
  uint <- list(c(1,2),c(2,3)) # valid interval input
  ufun <- list(function(x) x+1, function(x) 2*x-1) # valid function input
  out <- faux_Skx(uint,ufun)
  expect_equal(class(out), "list")
  
  # Test 2 - check that the output is a list of functions
  set.seed(0)
  uint <- list(c(1,2),c(2,3)) # valid interval input
  ufun <- list(function(x) x+1, function(x) 2*x-1) # valid function input
  out <- faux_Skx(uint,ufun)
  expect_equal(lapply(out,class),as.list(rep("function",length(ufun))))
  
  # Test 3 - check that the output is a list of the same length as the input
  # list of functions
  set.seed(0)
  uint <- list(c(1,2),c(2,3)) # valid interval input
  ufun <- list(function(x) x+1, function(x) 2*x-1) # valid function input
  out <- faux_Skx(uint,ufun)
  expect_that(length(out),equals(length(ufun)))
  
})