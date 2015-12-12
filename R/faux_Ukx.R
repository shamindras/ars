#' Helper function to get the upper bound linear function \eqn{u_{k}(x)}
#'
#' @param inp_xvec A vector of x values of all points
#' @param inp_gfun A function user wants to generate samples from. This function is
#'     used to calculate h(x)=ln(g(x))
#' @return the upper bound linear function \eqn{u_{k}(x)} for x in the interval
#' \eqn{[z_{j-1},z_{j}]} for j=1,...,k
#' @export
faux_Ukx <- function(inp_xvec, inp_gfun){
    #inp_gfun must be a function
    if(!is.function(inp_gfun)){
        stop("inp_gfun must be a valid R function")
    }
    #inp_xvec must be numeric and contains at least 2 elements
    if(length(inp_xvec) < 2 | !is.numeric(inp_xvec)){
        stop("inp_xvec must be at least 2 numeric elements")
    }
  
    #inp_xvec must be in increasing order
    inp_xvec <- sort(inp_xvec)

    # Get the expression of h(x)
    h_x <- faux_hx(inp_gfun)

    # Get the vector of h(x) applied to the vector x
    hx_inp_xvec <- h_x(inp_xvec)

    # Get the vector of h'(x) applied to the vector x
    hprimex_inp_xvec <- faux_hPrimex(inp_gfun = inp_gfun,
                                     inp_xvec = inp_xvec)
    
    # Get the functions for Uk(x)
    getUk <- function(a,b,c){
      f <- function(x) x
      body(f) <- substitute(a + (x - b) * c,
                            list(a = a,
                                 b = b,
                                 c = c))
      return(f)
    }

    #output the list of functions
    faux_Ukx_out <- Map(getUk,hx_inp_xvec,inp_xvec,hprimex_inp_xvec)
}


