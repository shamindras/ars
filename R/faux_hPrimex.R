#' Helper function to get first derivative of \eqn{h(x)}
#'
#' @param inp_gfun A function of \eqn{x} which the user wants to generate
#' samples from.
#' This function is used to calculate \eqn{h(x)=\log(g(x))}{h(x) = ln(g(x))}.
#' @param inp_xvec A number indicates the x-axis of the point
#' @return A numeric vector of length equal to \code{inp_xvec}.
#' The elements of the returned value are equal to the first derivative of
#' \eqn{h(x)} at the points in \code{inp_xvec}.
#' @export
faux_hPrimex <- function(inp_gfun, inp_xvec)
{
  #library('numDeriv')
  #inp_gfun must be a functionx
  if(!is.function(inp_gfun)){
    stop("inp_gfun must be a valid R function")
  }
  #inp_xvec must be numeric and contains at least 1 elements 
  if(length(inp_xvec) < 1 | !is.numeric(inp_xvec)){
    stop("inp_xvec must be at least 1 numeric elements")
  }
  h_x <- faux_hx(inp_gfun)
  hPrimex_out <- grad(h_x, inp_xvec)
  #If there is NaN values, add tolerence to regenerate the hPrimex_out
  if(any(is.nan(hPrimex_out))){
    #NaN position 
    pos <- which(is.nan(hPrimex_out))
    #change the h' to be the h' of the x+tolerence so that it gives the 
    #correct result 
    hPrimex_out[pos] <- grad(h_x,inp_xvec[pos]+def_faux_hPrime_Perturb)
  }
  return(hPrimex_out)
}