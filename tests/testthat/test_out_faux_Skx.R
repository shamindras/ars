context("test_out_faux_Skx: Check output of Skx is a list of functions")

test_that("Output of faux_Skx is a valid list of functions making up Skx", {
  
  # Test 1 - Check that the output is a list
  uint <- list(c(1,2),c(2,3)) # valid interval input
  ufun <- list(function(x) x+1, function(x) 2*x-1) # valid function input
  out <- faux_Skx(uint,ufun)
  expect_equal(class(out), "list")
  
  # Test 2 - check that the output is a list of functions
  uint <- list(c(1,2),c(2,3)) # valid interval input
  ufun <- list(function(x) x+1, function(x) 2*x-1) # valid function input
  out <- faux_Skx(uint,ufun)
  expect_equal(lapply(out,class),as.list(rep("function",length(ufun))))
  
  # Test 3 - check that the output is a list of the same length as the input
  # list of functions
  uint <- list(c(1,2),c(2,3)) # valid interval input
  ufun <- list(function(x) x+1, function(x) 2*x-1) # valid function input
  out <- faux_Skx(uint,ufun)
  expect_that(length(out),equals(length(ufun)))
  
})