#' Helper function to get function \eqn{s_k(x)}
#' 
#' @param inp_x A vector of inp_x values of all points
#' @param z A vector of z values of all points and we should be able to get the 
#'   index of z
#' @param g A function user wants to generate samples from. This function is 
#'   used to calculate h(x)=ln(g(x))
#' @param D a numeric vector indicating the domain of the g function. It is
#' possible to specify D=c(-Inf,Inf).
#' @return the function $s_{k}(x)$ for all x in the domain of g.
#' @export

sFun <- function(x,inp_x,z,g,D) { 
  f <- function(y) exp(uFun(x=y,inp_x=inp_x,z=z,g=g,D=D))
  f(y=x)/(integrate(f,lower=D[1],upper=D[2])[[1]])
}
