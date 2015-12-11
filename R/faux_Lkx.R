#' Helper function to get the lower bound linear function $l_{k}(x)$
#'
#' @param inp_xvec A vector of x values of all points and we should be able to get the
#' index of x
#' @param inp_gfun A function user wants to generate samples from. This function is
#' use to calculate h(x)=ln(g(x))
#' @return the lower bound linear function $l_{k}(x)$ for x in the interval
#' [$x_{j}$,$x_{j+1}$]
#' @export
faux_Lkx <- function(inp_xvec,inp_gfun) {
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
  #sort vector of x in increasing order 
  inp_xvec <- sort(inp_xvec)
  #length of inp_xvec 
  k <- length(inp_xvec)
  #vector a contains x1,x2,...x_{k-1}  
  vec_a <- inp_xvec[1:(k-1)]
  #vector b contains x2,x3,...x_k
  vec_b <- inp_xvec[2:k]
  #get the denominator in the function expresssion which is x_{k}-x_{k-1}
  vec_denominator <- vec_b - vec_a
  #Get the expression of h(x)
  h_x <- faux_hx(inp_gfun)
  #Get the expression of the function lk 
  getLk <- function(i){
    str <- "((vec_b[i]-x)*h_x(vec_a[i])+(x-vec_a[i])*h_x(vec_b[i]))/vec_denominator[i]"
    Lkx <- function(x) eval(parse(text=str))
    return(Lkx)
  }
  #initialize faux_Lkx_out 
  faux_Lkx_out <- c()
  #if x<x1, treat it as the first interval [-Inf, x1], the function is -Inf 
  faux_Lkx_out[1] <- def_faux_Lkx_negInf 
  #if x>xk, treat it as the last interval [xk,Inf], the function is also -Inf
  faux_Lkx_out[k+1] <- def_faux_Lkx_negInf 
  #output the list of functions 
  faux_Lkx_out[2:k] <- sapply(1:length(vec_a),getLk)
  #if x<x1, treat it as the first 
  return(as.list(faux_Lkx_out))
}



