#' Helper function to get the intersection of tangents at $x_{j}$ and $x_{j+1}$
#'
#' @param inp_xvec A vector of x values of your points. The vector is ordered in an
#' increasing order.
#' @param inp_gfun A fucntion user wants to generate samples from. This function is
#' used to calculate h(x)=ln(g(x))
#' @param inp_Dvec A vector with 2 elements indicating the domain the function g
#' @return a vector of intersection of tangents
#' @export

#library('Oarray')
faux_Zj <- function(inp_xvec, inp_gfun, inp_Dvec)
{
  #get the length of the vector x
  k <- length(inp_xvec)
  #initialize z as an empty array with first index 0 instead of 1
  z <- Oarray(dim=k+1,offset = 0)
  #set the first element of vector z to be lower bound of inp_Dvec
  z[0] <- inp_Dvec[1]
  #set the last element of vector z to be upper bound of inp_Dvec
  z[k] <- inp_Dvec[2]
  #create 2 vectors
  a <- inp_xvec[1:(k-1)]
  b <- inp_xvec[2:k]
  calZj <- function(a,b)
  {
    h_x <- faux_hx(inp_gfun)
    hPrimeb <- faux_hPrimex(inp_gfun,b)
    hPrimea <- faux_hPrimex(inp_gfun,a)
    numerator <- h_x(b) - h_x(a) - b*hPrimeb + a*hPrimea
    denominator <- hPrimea - hPrimeb
    return(numerator/denominator)
  }
  z[1:(k-1)] <- mapply(calZj,a,b)
  return(z)
}