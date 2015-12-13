#' Helper function to check the log concavity
#' 
#' @param inp_xvec A vector of x values
#' \eqn{(x_{1}, x_{2}, x_{3})}{(x1, x2, x3)}
#'    in the domain of the user supplied function \eqn{g(x)}
#' @param inp_gfun A function user wants to generate samples from. This function is 
#'    used to calculate \eqn{h(x)=\log(g(x))}{h(x) = ln(g(x))}.
#'    We can then test the log-concavity
#'    at the 3 \eqn{x}-coordinates \eqn{(x_{1}, x_{2}, x_{3})}{(x1, x2, x3)}
#' @return A logical vector of length 1:
#'    TRUE if the slope of the secant between the first two points is
#'    greater than or equal to the slope of the secant between the second
#'    two points; FALSE otherwise. 
#' @export
faux_CheckLogConcavity <- function(inp_xvec, inp_gfun){ 
  
  # inp_gfun must be a function
  if(!is.function(inp_gfun)){
    stop("inp_gfun must be a valid R function")
  }
  
  # inp_xvec must be numeric and contain exactly 3 elements 
  if(length(inp_xvec) != 3 | !is.numeric(inp_xvec)){
    stop("inp_xvec must be exactly 3 numeric elements")
  }

  # inp_xvec must be numeric and contain exactly 3 elements 
  if(length(unique(inp_xvec)) != 3){
    stop("inp_xvec must be exactly 3 unique numeric elements")
  }
  
  # inp_xvec must be in ascending order
  inp_xvec <- sort(inp_xvec)
  
  # Create h function from the g function that is input by the user
  h_x <- faux_hx(inp_gfun)
  
  # Evaluate h at each value of x in the x vector parameter
  y <- h_x(inp_xvec)
  
  # Calculate the gradients between the first two points and the last two points
  g1 <- (y[2] - y[1]) / (inp_xvec[2] - inp_xvec[1])
  g2 <- (y[3] - y[2]) / (inp_xvec[3] - inp_xvec[2])
  
  # Check if the first gradient is greater than or equal to the second,
  # which will be the case if h is concave
  CheckLogConcavity_out <- ifelse(identical(all.equal(g1, g2), TRUE)
                                                   , TRUE
                                                   , ifelse(g1 - g2 >= 0
                                                            , TRUE
                                                            , FALSE))
  
  return(CheckLogConcavity_out)
}
