context("test_inp_faux_InitChoose: Check Choice of initial 3 Sample Points")

test_that("Inputs are Validated", {

 # Test 1
 g    <- function(x) x^2 # valid function 
 Dvec <- c(10, 11, 3)    # invalid vector should only have 2 elements!
 # Expected error
 expect_error(faux_InitChoose3(inp_gfun = g, inp_Dvec = Dvec))

 # Test 2
 g    <- "x^2"           # invalid function 
 Dvec <- c(10, 11)       # valid vector should only have 2 elements!
 # Expected error
 expect_error(faux_InitChoose3(inp_gfun = g, inp_Dvec = Dvec))

 # Test 3
 g    <- function(x) x^2 # valid function 
 Dvec <- c(10, "a")      # invalid vector - must be numeric
 # Expected error
 expect_error(faux_InitChoose3(inp_gfun = g, inp_Dvec = Dvec))

 # Test 4
 g    <- "x^2"           # invalid function 
 Dvec <- c(10, "a")      # invalid vector - must be numeric
 # Expected error
 expect_error(faux_InitChoose3(inp_gfun = g, inp_Dvec = Dvec))

 # Test 5
 g    <- function(x) x^2 # valid function 
 Dvec <- c(11, 10)       # invalid vector first element larger than second
 # Expected error
 expect_error(faux_InitChoose3(inp_gfun = g, inp_Dvec = Dvec))

 # Test 6
 g    <- function(x) x^2 # valid function 
 Dvec <- c(11, 10)       # invalid vector first element equal to the second
 # Expected error
 expect_error(faux_InitChoose3(inp_gfun = g, inp_Dvec = Dvec))


})