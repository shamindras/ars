#' Helper function to get \eqn{h(x) = \log(g(x))}.
#'
#' @param inp_gfun A function of \eqn{x} which the user wants to generate samples from.
#' This function is used to calculate \eqn{h(x)=\log(g(x))}.
#' @return the \eqn{h(x)}
#' @export
faux_hx <- function(inp_gfun){
  
  #inp_gfun must be a function
  if(!is.function(inp_gfun)){
    stop("inp_gfun must be a valid R function")
  }
  
  # Define new function as log of input function
  faux_hx_out <- function(x) log(inp_gfun(x))
  
  return(faux_hx_out)
}
