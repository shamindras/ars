#' Helper function to get the intersection of tangents at $x_{j}$ and $x_{j+1}$
#'
#' @param inp_xvec A vector of x values of your points. The vector is ordered in
#' an increasing order.
#' @param inp_gfun A fucntion user wants to generate samples from. This function 
#' is used to calculate h(x)=ln(g(x))
#' @param inp_Dvec A vector with 2 elements indicating the domain the function g
#' @return a vector of intersection of tangents
#' @export
faux_Zj <- function(inp_xvec, inp_gfun, inp_Dvec)
{
  # Input validation with Error handling
  #Domain must be numeric and contains 2 elements in the vector
  if(length(inp_Dvec) != 2 | !is.numeric(inp_Dvec)){
    stop("inp_Dvec must be 2 numeric elements")
  }
  #Domian must be 2 numeric elements and the first element is smaller
  if(length(inp_Dvec) == 2 & is.numeric(inp_Dvec) & (inp_Dvec[1] >= inp_Dvec[2])){
    stop("inp_Dvec must be 2 numeric elements, first element smaller than
         the second element")
  }
  #inp_gfun must be a function
  if(!is.function(inp_gfun)){
    stop("inp_gfun must be a valid R function")
  }
  #inp_xvec must be numeric and contains at least 2 elements 
  if(length(inp_xvec) < 2 | !is.numeric(inp_xvec)){
    stop("inp_xvec must be at least 2 numeric elements")
  }
  #inp_xvec must be increasing order 
  if(any(inp_xvec-sort(inp_xvec)!=0)){
    stop("inp_xvec must be increasing order")
  }
  #get the length of the vector x
  k <- length(inp_xvec)
  #initialize z as an empty vector
  out_Zvec <- c()
  #set the first element of vector z to be lower bound of inp_Dvec
  out_Zvec[1] <- inp_Dvec[1]
  #set the last element of vector z to be upper bound of inp_Dvec
  out_Zvec[k+1] <- inp_Dvec[2]
  #create 2 vectors
  #vector a contains x1,x2,...x_{k-1}  
  vec_a <- inp_xvec[1:(k-1)]
  #vector b contains x2,x3,...x_k
  vec_b <- inp_xvec[2:k]
  #Get the expression of h(x)
  h_x <- faux_hx(inp_gfun)
  #get the vector of h'(a)
  vec_hPrimea <- faux_hPrimex(inp_gfun,vec_a)
  #get the vector of h'(b)
  vec_hPrimeb <- faux_hPrimex(inp_gfun,vec_b)
  #get the numerator of z_{k},k is in the range [2:k]
  vec_numerator <- h_x(vec_b) - h_x(vec_a) - vec_b*vec_hPrimeb + vec_a*vec_hPrimea
  #get the denominator of z_{k}, k is in the range [2:k]
  vec_denominator <- vec_hPrimea - vec_hPrimeb
  #set z[2:k] to be numerator/denominator 
  out_Zvec[2:k] <- vec_numerator/vec_denominator 
  #return values of z as a vector 
  return(out_Zvec)
}