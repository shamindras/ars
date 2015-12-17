context("test-out_faux_uInterval: Check output of uInterval")

test_that("test-out_faux_uInterval: Outputs are Validated", {
  
  # Test 1 - Check that the output is a list
  set.seed(0)
  zvals <- c(1,2,4,6) # valid input for z values
  out <- faux_uInterval(zvals)
  expect_equal(class(out), "list")
  
  # Test 2 - Check that the output is a list
  set.seed(0)
  zvals <- c(Inf,1,2,4,6,-Inf) # valid input for z values, note it is unsorted
  out <- faux_uInterval(zvals)
  expect_equal(class(out), "list")
  
  # Test 3 - check that the output is a list of intervals/vectors
  set.seed(0)
  zvals <- c(1,2,4,6) # valid input for z values
  out <- faux_uInterval(zvals)
  expect_equal(lapply(out,class),as.list(rep("numeric",length(zvals)-1)))
  
  # Test 4 - check that the length of the output, which is a list of intervals,
  # is one less than the length of the input, since if there are n points there
  # there are n-1 intervals in between them
  set.seed(0)
  zvals <- c(-Inf,1,2,6,4,Inf) # valid input for z values, unsorted
  out <- faux_uInterval(zvals)
  expect_that(length(out),equals(length(zvals)-1))
  
})