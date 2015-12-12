#' Helper function to check if the point is accepted as one of the generating
#' samples 
#' 
#' @param inp_xvec A vector of x values of all points 
#' @param inp_zvec A vector of z values of all points and we should be able to get 
#'     the index of z
#' @param inp_gfun A function user wants to generate samples from. This function is 
#'     used to calculate \eqn{h(x)=\log(g(x))}
#' @return A vector of boolean value TRUE or FALSE indicating whether it's 
#'     accepted and the value of \eqn{x^{*}}
#' @export
faux_AcceptRejectx <- function(inp_xvec, inp_zvec, inp_gfun){

}
