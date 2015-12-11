#' Helper function to create the z intervals.
#' @param inp_z A vector of z values, such as the output from faux_Zj().
#' @return  A list, with each element of the list an interval between 2 z points
#' @export

faux_uInterval <- function(inp_z) {
  
  # check that inp_z is a vector
  if(!(class(inp_z)=="numeric")){
    stop("inp_z must be a vector of z values")
  }
  
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