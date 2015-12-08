#' Helper function to check if the point is accepted as one of the generating
#' samples 
#' @param x A vector of x values of all points 
#' @param z A vector of z values of all points and we should be able to get 
#' the index of z
#' @param g A function user wants to generate samples from. This function is 
#' used to calculate h(x)=ln(g(x))
#' @return A vector of boolean value TRUE or FALSE indicating whether it's 
#' accepted and the value of x*
#' @export
checkAccept <- function(x,z,g) { }
