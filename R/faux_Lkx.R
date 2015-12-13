#' Helper function to get the lower bound linear function \eqn{l_{k}(x)}
#'
#' @param inp_xvec A vector of x values of all points and we should be able to get the
#' index of \eqn{x}
#' @param inp_gfun A function user wants to generate samples from. This function is
#' use to calculate \eqn{h(x)=\log(g(x))}
#' @return the lower bound linear function \eqn{l_{k}(x)} for \eqn{x} in the interval
#' \eqn{[x_{j},x_{j+1}]}
#' @export
faux_Lkx <- function(inp_xvec, inp_gfun) {
  
  #inp_gfun must be a function
  if(!is.function(inp_gfun)){
    stop("inp_gfun must be a valid R function")
  }
  #inp_xvec must be numeric and contains at least 2 elements 
  if(length(inp_xvec) < 2 | !is.numeric(inp_xvec)){
    stop("inp_xvec must be at least 2 numeric elements")
  }
  
  #sort vector of x in increasing order 
  inp_xvec <- sort(inp_xvec)
  
  #length of inp_xvec 
  k <- length(inp_xvec)
  
  #this vector contains x1,x2,...x_{k-1}; it is analogous to x_j in the formula
  vec_xj <- inp_xvec[1:(k-1)]
  
  #this vector contains x2,x3,...x_k; it is analogous to x_{j+1} in the formula
  vec_xj1 <- inp_xvec[2:k]
  
  #Get the expression of h(x)
  h_x <- faux_hx(inp_gfun)
  
  #Get values of h(x_j)
  vec_hxj <- h_x(vec_xj)
  
  #Get values of h(x_{j+1})
  vec_hxj1 <- h_x(vec_xj1)
  
  #Function to create Lk function on each interval
  getLk <- function(a,b,c,d){
    f <- function(x) x
    body(f) <- substitute(((a - x) * b + (x - c) * d) / (a - c),
                          list(a = a,
                               b = b,
                               c = c,
                               d = d))
    return(f)
  }
  
  #Create list of functions
  faux_Lkx_out <- Map(getLk,vec_xj1,vec_hxj,vec_xj,vec_hxj1)
  
  #if x<x1, treat it as the first interval [-Inf, x1], the function is -Inf 
  #if x>xk, treat it as the last interval [xk,Inf], the function is also -Inf
  faux_Lkx_out <- c(function(x) def_faux_Lkx_negInf,
                    faux_Lkx_out,
                    function(x) def_faux_Lkx_negInf)
  
  return(faux_Lkx_out)
}



