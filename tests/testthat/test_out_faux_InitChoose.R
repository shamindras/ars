context("test_inp_faux_InitChoose: Check Initially chosen 2 sample points are reasonable")

test_that("Outputs are Validated", {

 # Test 1 - Check that we correctly sample 2 points as a default
 g    <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function 
 Dvec <- c(-Inf, Inf)                               # valid Support
 y_test <- faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec)
 expect_equal(length(y_test$init_sample_points), 2)

 # Test 2 - Check that we correctly sample 4 points if specified
 #          Should pass as 4 points is an even integer
 g    <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function 
 Dvec <- c(-Inf, Inf)                               # valid Support
 y_test <- faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec, inp_Initnumsampvec = 4)
 expect_equal(length(y_test$init_sample_points), 4)

 # Test 3 - Check that we get an error if we try and initialise with an
 #          a positive decimal number instead of a positive even integer
 g    <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function 
 Dvec <- c(-Inf, Inf)                               # valid Support 
 expect_error(faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec
              , inp_Initnumsampvec = 3.5))


})