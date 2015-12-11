#' Helper function to check the log concavity
#' 
#' @param inp_xvec A vector of 3 consecutive x values $(x_{1}, x_{2}, x_{3})$
#'    in the domain of the user supplied function $g(x)$   
#' @param inp_gfun A function user wants to generate samples from. This function is 
#'    used to calculate h(x)=ln(g(x)). We can then test the log-concavity
#'    at the 3 x-coordinates $(x_{1}, x_{2}, x_{3})$
#' @return boolean value TRUE or FALSE to indicate the log concavity 
#' @export
faux_CheckLogConcavity <- function(inp_xvec, inp_gfun){ 
}
