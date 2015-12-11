#' Helper function to get first derivative of h(x)
#'
#' @param inp_gfun A function of x which the user wants to generate samples from.
#' This function is used to calculate h(x)=ln(g(x)).
#' @param inp_xvec A number indicates the x-axis of the points
#' @return the first derivative of h(x) at the point x
#' @export
faux_hPrimex <- function(inp_gfun, inp_xvec)
{
  #inp_gfun must be a function
  if(!is.function(inp_gfun)){
    stop("inp_gfun must be a valid R function")
  }
  #inp_xvec must be numeric and contains at least 1 elements 
  if(length(inp_xvec) < 1 | !is.numeric(inp_xvec)){
    stop("inp_xvec must be at least 1 numeric elements")
  }
  h_x <- faux_hx(inp_gfun)
  # the derivative is approximately (h(inp_xvec+faux_hPrimex_tol)-h(inp_xvec)) /
  #                                 faux_hPrimex_tol
  hPrimex_out <- (h_x(inp_xvec+faux_hPrimex_tol)-h_x(inp_xvec)) / faux_hPrimex_tol
  return(hPrimex_out)
}