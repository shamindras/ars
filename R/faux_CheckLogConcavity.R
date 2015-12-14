#' Helper function to check the log concavity
#' 
#' @param inp_gfun A function user wants to generate samples from. This function is 
#'    used to calculate \eqn{h(x)=\log(g(x))}{h(x) = ln(g(x))}.
#' @param inp_Dvec A numeric vector of length 2 indicating the support of
#'    \code{inp_gfun}.
#' @return A logical vector of length 1:
#'    \code{TRUE} if \code{inp_gfun} is log-concave,
#'    \code{FALSE} if \code{inp_gfun} is not log-concave.
#' @export
faux_CheckLogConcavity <- function(inp_gfun, inp_Dvec){ 
  
  # inp_gfun must be a function
  if(!is.function(inp_gfun)){
    stop("inp_gfun must be a valid R function")
  }
  
  # Check that the user enters a support with exactly 2 elements
  if(length(inp_Dvec) != 2 | !is.numeric(inp_Dvec)){
    stop("inp_Dvec must be 2 numeric elements")
  }

  # inp_Dvec must contain only unique elements 
  if(length(unique(inp_Dvec)) != length(inp_Dvec)){
    stop("inp_xvec must be contain unique numeric elements")
  }
  
  # inp_Dvec must be in ascending order
  inp_Dvec <- sort(inp_Dvec)
  
  # Create an interval in the domain that is small enough for testing
  # log-concavity
  smallD <- numeric(2)
  if (is.infinite(inp_Dvec[1])) {
    smallD[1] <- def_faux_CheckLogConcavity_Dmin
  } else {
    smallD[1] <- max(inp_Dvec[1],def_faux_CheckLogConcavity_Dmin)
  }
  if (is.infinite(inp_Dvec[2])) {
    smallD[2] <- def_faux_CheckLogConcavity_Dmax
  } else {
    smallD[2] <- min(inp_Dvec[2],def_faux_CheckLogConcavity_Dmax)
  }
  
  # smallD must be in ascending order
  smallD <- sort(smallD)
  
  # Create a vector of points to test for log-concavity
  test <- seq(from = smallD[1],
              to = smallD[2],
              length.out = def_faux_CheckLogConcavity_nPoints)
  # What if the domain is really small? With a standard number of points to test,
  # if the domain is really small then the points might be so close together that
  # the secants between them have virtually equivalent gradients, even if
  # the function is not log-concave.
  
  # Create h function from the g function that is input by the user
  h_x <- faux_hx(inp_gfun)
  
  # Create offset x vectors
  x1 <- test[-length(test)]
  x2 <- test[-1]
  
  # Evaluate h at each value of x in the offset x vectors
  y1 <- h_x(x1)
  y2 <- h_x(x2)
  
  # Calculate the gradients of the secants between consecutive points
  gr <- (y2 - y1) / (x2 - x1)
  
  # Check if the gradients are decreasing, within machine tolerance
  out <- isTRUE(all.equal(gr,cummin(gr)))
  
  return(out)
  
}
