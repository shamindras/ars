context("test_out_faux_SampleSkx: Check that the output is valid")

test_that("Output is a valid number sampled from Skx", {
  
  # Test 1 - Check that the output is numeric
  uint <- list(c(-1,0),c(0,1)) # valid interval input
  # valid input of skx function
  inp_sfunlist <- faux_Skx(list(c(-1,0),c(0,1)),
                           list(function(x) x+3, function(x) 3-x)) 
  out <- faux_SampleSkx(uint,inp_sfunlist)
  expect_equal( class(out), "numeric")
  
  # Test 2 - Check that the output is not NA
  uint <- list(c(-1,0),c(0,1)) # valid interval input
  # valid input of skx function
  inp_sfunlist <- faux_Skx(list(c(-1,0),c(0,1)),
                           list(function(x) x+3, function(x) 3-x))
  out <- faux_SampleSkx(uint,inp_sfunlist)
  expect_that(is.na(out),is_false())
  
})