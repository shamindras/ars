#' Helper function to get first derivative of h(x)
#'
#' @param inp_gfun A function of x which the user wants to generate samples from.
#' This function is used to calculate h(x)=ln(g(x)).
#' @param inp_xvec A number indicates the x-axis of the points
#' @return the first derivative of h(x) at the point x
#' @export
faux_hPrimex <- function(inp_gfun, inp_xvec)
{
  h_x <- faux_hx(inp_gfun)
  # the derivative is approximately (h(inp_xvec+faux_hPrimex_tol)-h(inp_xvec)) /
  #                                 faux_hPrimex_tol
  hPrimex_out <- (h_x(inp_xvec+faux_hPrimex_tol)-h_x(inp_xvec)) / faux_hPrimex_tol
  return(hPrimex_out)
}