context("test-inp_faux_InitChoose: Check Choice of initial 2 Sample Points")

test_that("test-inp_faux_InitChoose: Inputs are Validated", {

 # Test 1
 set.seed(0)
 g    <- function(x) x^2 # valid function 
 Dvec <- c(10, 11, 3)    # invalid vector should only have 2 elements!
 # Expected error
 expect_error(faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec))

 # Test 2
 set.seed(0)
 g    <- "x^2"           # invalid function 
 Dvec <- c(10, 11)       # valid vector should only have 2 elements!
 # Expected error
 expect_error(faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec))

 # Test 3
 set.seed(0)
 g    <- function(x) x^2 # valid function 
 Dvec <- c(10, "a")      # invalid vector - must be numeric
 # Expected error
 expect_error(faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec))

 # Test 4
 set.seed(0)
 g    <- "x^2"           # invalid function 
 Dvec <- c(10, "a")      # invalid vector - must be numeric
 # Expected error
 expect_error(faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec))

 # Test 5
 set.seed(0)
 g    <- function(x) x^2 # valid function 
 Dvec <- c(11, 10)       # invalid vector first element larger than second
 # Expected error
 expect_error(faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec))

 # Test 6
 set.seed(0)
 g    <- function(x) x^2 # valid function 
 Dvec <- c(11, 10)       # invalid vector first element equal to the second
 # Expected error
 expect_error(faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec))

})