#' Helper function to get function \eqn{s_k(x)}
#' @param z_int A vector of z values of all points and we should be able to get 
#' the index of z
#' @param uFun_out A list of functions, the output from uFun.
#' @return the function $s_{k}(x)$ for all x in the domain of g.
#' @export

sFun <- function(z_int,uFun_out) {
  c <- function(i) integrate(uFun_out[[i]],z_int[[i]][1],z_int[[i]][2])[[1]]
  constant <- sum(sapply(seq(1:length(z_int)),c))
}
