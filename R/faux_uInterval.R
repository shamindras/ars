#' Helper function to create the z intervals.
#' @param inp_z A vector of z values, such as the output from faux_Zj().
#' @return  A list, with each element of the list an interval between 2 z points
#' @export

faux_uInterval <- function(inp_z) {
  z <- cbind(inp_z[1:(length(inp_z)-1)],inp_z[2:length(inp_z)])
  faux_uInterval_out <- as.list(data.frame(t(z)))
  return(faux_uInterval_out)
}