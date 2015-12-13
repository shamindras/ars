#' Helper function to create the \eqn{z} intervals.
#' @param inp_z A vector of \eqn{z} values, such as the output from
#' \code{faux_Zj()}.
#' @return  A list of numeric vectors, each of length 2.
#' The length of the list is one less than the length of the input \code{inp_z}.
#' Each element of the list is an interval between 2 consecutive \eqn{z} points.
#' @export

faux_uInterval <- function(inp_z) {
  
  # check that inp_z is a vector
  if(!(class(inp_z)=="numeric")){
    stop("inp_z must be a vector of z values")
  }
  
  inp_z <- sort(inp_z)
  
  # form a matrix with 2 columns where each column is an enpoint of an interval
  z <- cbind(inp_z[1:(length(inp_z)-1)],inp_z[2:length(inp_z)])
  
  # check that the points in inp_z are in increasing order
  if(!(sum(z[,1]<z[,2])==dim(z)[1])){
    stop("inp_z must be a vector of z points in increasing order")
  }
  
  # create a list where each element of the list is a row from the matrix
  faux_uInterval_out <- as.list(data.frame(t(z)))
  names(faux_uInterval_out) <- NULL
  return(faux_uInterval_out)
}