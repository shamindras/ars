context("test_out_faux_InitChoose: Check Initially
        chosen 2 sample points are reasonable")

test_that("test_out_faux_InitChoose: Outputs are Validated", {

 # Test 1 - Check that we correctly sample 2 points as a default
 g    <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function 
 Dvec <- c(-Inf, Inf)                               # valid Support
 y_test <- faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec)
 expect_equal(length(y_test$init_sample_points), 2)

 # Test 2 - Check that we correctly sample 4 points if specified
 #          Should pass as 4 points is an even integer
 g    <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function 
 Dvec <- c(-Inf, Inf)                               # valid Support
 y_test <- faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec, 
                           inp_Initnumsampvec = 4)
 expect_equal(length(y_test$init_sample_points), 4)

 # Test 3 - Check that we get an error if we try and initialise with
 #          a positive decimal number instead of a positive even integer
 g    <- function(x) dnorm(x = x, mean = 0, sd = 1) # valid function 
 Dvec <- c(-Inf, Inf)                               # valid Support 
 # expect_that(faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec
 #             , inp_Initnumsampvec = 3.5), throws_error()) 
 expect_error(faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec
              , inp_Initnumsampvec = 3.5))
 
 # Test 4 Check that the mode found in the function is correct for the standard
 # normal distribution
 g <- function(x) dnorm(x) # valid function
 Dvec <- c(-Inf, Inf) # valid Support 
 out <- faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec)
 expect_equal(out$mode,0, tolerance=.00001)
 
 # Test 5 Check that the mode found in the function is correct for the chisquare
 # distribution with 5 df, which means the mode should be 2.
 g <- function(x) dchisq(x,10) # valid function
 Dvec <- c(0, Inf) # valid Support 
 out <- faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec)
 expect_equal(out$mode,8, tolerance=.00001)
 
 # Test 6 Check that the points chosen have correcty sloped tangent lines for 
 # the standard normal distribution
 g <- function(x) dnorm(x) # valid function
 Dvec <- c(-Inf, Inf) # valid Support 
 out <- faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec)
 expect_that(faux_hPrimex(function(x) dnorm(x),out$init_sample_points[1])>0,
             is_true())
 expect_that(faux_hPrimex(function(x) dnorm(x),out$init_sample_points[2])<0,
             is_true())
 
 # Test 7 Check that the points chosen have correcty sloped tangent lines
 g <- function(x) {2*exp(-2*x)} # valid function
 Dvec <- c(0, Inf) # valid Support 
 out <- faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec)
 expect_that(faux_hPrimex(function(x) 2*exp(-2*x),out$init_sample_points[2])<0,
             is_true())
 
 # Test 8 Check that the points chosen have correcty sloped tangent lines for
 # the chisquare distribution with df=5
 g <- function(x) dchisq(x, df=5) # valid function
 Dvec <- c(0, Inf) # valid Support 
 out <- faux_InitChoose(inp_gfun = g, inp_Dvec = Dvec)
 expect_that(faux_hPrimex(function(x) 2*exp(-2*x),out$init_sample_points[2])<0,
             is_true())
 

# UPDATE: Come back and finish this!
# inp_gfun    <- function(x) dnorm(x = x, mean = 7000, sd = 45)
# inp_gfun    <- function(x) {2 - (x-5)^2}
# inp_gfun    <- function(x) {2*exp(-2*x)}
# inp_Dvec <- c(0, Inf)
# test_faux_InitChoose <- faux_InitChoose(inp_gfun = inp_gfun, inp_Dvec = inp_Dvec)


})