#' Helper function to get the intersection of tangents at $x_{j}$ and $x_{j+1}$
#' 
#' @param x A vector of x values of your points. The vector is ordered in an
#' increasing order.
#' @param g A fucntion user wants to generate samples from. This function is 
#' used to calculate h(x)=ln(g(x))
#' @param D A vector with 2 elements indicating the domain the function g
#' @return a vector of intersection of tangents 
#' @export

library('Oarray')
faux_Zj <- function(x,g,D) 
{
  #get the length of the vector x
  k <- length(x)
  #initialize z as an empty array with first index 0 instead of 1 
  z <- Oarray(dim=k+1,offset = 0)
  #set the first element of vector z to be lower bound of D 
  z[0] <- D[1]
  #set the last element of vector z to be upper bound of D 
  z[k] <- D[2]
  #create 2 vectors 
  a <- x[1:(k-1)]
  b <- x[2:k]
  calZj <- function(a,b)
  {
    h_x <- faux_hx(g)
    hPrimeb <- faux_hPrimex(g,b)
    hPrimea <- faux_hPrimex(g,a)
    numerator <- h_x(b) - h_x(a) - b*hPrimeb + a*hPrimea
    denominator <- hPrimea - hPrimeb
    return(numerator/denominator)
  }
  z[1:(k-1)] <- mapply(calZj,a,b)
  return(z)
}