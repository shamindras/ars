context("faux_Skx: Check Choice of initial 3 Sample Points")

test_that("Inputs are Validated", {

 # Test 1
 # g    <- function(x) x^2 # valid function 
 # Dvec <- c(10, 11, 3)    # invalid vector should only have 2 elements!
 # Expected error
 expect_equal(1+1, 2)

 expect_equal(1+1, 3)
})