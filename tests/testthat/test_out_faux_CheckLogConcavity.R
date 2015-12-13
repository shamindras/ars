context("test_out_faux_CheckLogConcavity: Check validity of output")

test_that("test_out_faux_CheckLogConcavity: Outputs are Validated", {
  
  # Test 1 - Check that only 1 value is output
  xvec <- c(-1,0,1)                         # valid vector
  out <- faux_CheckLogConcavity(xvec,dnorm) # valid density
  expect_equal(length(out), 1)
  
  # Test 2 - Check that value is Boolean
  xvec <- c(-1,0,1)                         # valid vector
  out <- faux_CheckLogConcavity(xvec,dnorm) # valid density
  expect_equal(class(out),"logical")

})
