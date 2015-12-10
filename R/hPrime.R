#' Helper function to get first derivative of h(x)
#' 
#' @param inp_g A function of x which the user wants to generate samples from. This function is 
#' used to calculate h(x)=ln(g(x)).
#' @param x A number indicates the x-axis of the points 
#' @return the first derivative of h(x) at the point x 
#' @export
hPrime <-function(inp_g,x) {
  inp_g(x)+2
}