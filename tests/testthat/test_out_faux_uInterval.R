context("test_out_faux_uInterval: Check output of uInterval")

test_that("Outputs are Validated", {
  
  # Test 1 - Check that the output is a list
  zvals <- c(1,2,4,6) # valid input for z values
  out <- faux_uInterval(zvals)
  expect_equal(class(out), "list")
  
  # Test 2 - check that the output is a list of intervals/vectors
  zvals <- c(1,2,4,6) # valid input for z values
  out <- faux_uInterval(zvals)
  expect_equal(lapply(out,class),as.list(rep("numeric",length(zvals)-1)))
  
})