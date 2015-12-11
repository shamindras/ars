#' Helper function to create piecewise function {s_k(x)}
#' @param uInterval_out A list of intervals between z values, as in the output
#' from uInterval.
#' @param uFun_out A list of functions, the output from uFun.
#' @return the function $s_{k}(x)$ for all x in the domain of g, in list format,
#' where each element of the list is one element of the piecewise function.
#' @export

faux_Skx <- function(uInterval_out,uFun_out) {
  addexp <- function(i){
    str  <- deparse(body(uFun_out[[i]]))
    h <- function(x) eval(parse(text = paste0("exp(", str, ")")))
    return(h)
  }
  exps <- sapply(seq(1:length(uFun_out)),addexp)
  int <- function(i) integrate(exps[[i]], uInterval_out[[i]][1], uInterval_out[[i]][2])[[1]]
  constant <- sum(sapply(seq(1:length(uFun_out)),int))
  addconst <- function(i){
    str  <- deparse(body(uFun_out[[i]]))
    h <- function(x) eval(parse(text = paste0("exp(", str, ")/",constant)))
    return(h)
  }
  final <- sapply(seq(1:length(uFun_out)),addconst)
  return(final)
}

