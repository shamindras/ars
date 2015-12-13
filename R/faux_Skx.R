#' Helper function to create piecewise function \eqn{s_{k}(x)}{sk(x)}
#' @param inp_uintervallist A list of intervals between \eqn{z} values, as in the 
#' output from uInterval.
#' @param inp_ufunlist A list of functions, the output from uFun.
#' @return A list of functions.
#' The length of the list is equal to the length of the inputs
#' \code{inp_uintervallist} and \code{inp_ufunlist}.
#' Each element of the list is one piece of the piecewise function
#' \eqn{s_{k}(x)}{sk(x)}.
#' @export
faux_Skx <- function(inp_uintervallist, inp_ufunlist) {
  
  # check that inp_uintervallist is the same length as inp_ufunlist
  if(!(length(inp_ufunlist)==length(inp_uintervallist))){
    stop("inp_uintervallist and inp_ufunlist must have the same length")
  }
  
  # check that inp_uintervallist is a list
  if(!(class(inp_uintervallist))=="list"){
    stop("inp_uintervallist must be a list")
  }
  
  # check that inp_ufunlist is a list
  if(!(class(inp_ufunlist))=="list"){
    stop("inp_ufunlist must be a list")
  }
  
  # check that each element of inp_uintervallist is a 2-dim vector
  if(!(do.call(sum,lapply(inp_uintervallist,length)))==
     2*length(inp_uintervallist)){
    stop("Intervals in inp_uintervallist must be two dimensional vectors")
  }
  
  # check that every element of inp_ufunlist is a function
  if(!all.equal(lapply(inp_ufunlist,class),
                as.list(rep("function",length(inp_ufunlist))))){
    stop("inp_ufunlist must be a list of functions")
  }
  
  # function to take the exp() of every function in inp_ufunlist
  addexp <- function(i){
    str  <- deparse(body(inp_ufunlist[[i]]))
    h <- function(x) eval(parse(text = paste0("exp(", str, ")")))
    return(h)
  }
  
  exps <- sapply(seq(1:length(inp_ufunlist)),addexp) # exponentiate each 
  # function in the list
  
  # function to take the integral of each element in exps
  int <- function(i) integrate(exps[[i]], inp_uintervallist[[i]][1]
                               , inp_uintervallist[[i]][2])[[1]]
  
  constant <- sum(sapply(seq(1:length(inp_ufunlist)),int)) #normalizing constant
  
  #  function which exponentiates each function in inp_ufunlist and divides 
  # each by the normalizing constant
  addconst <- function(i){
    str  <- deparse(body(inp_ufunlist[[i]]))
    h <- function(x) eval(parse(text = paste0("exp(", str, ")/",constant)))
    return(h)
  }
  
  # final list of sk(x) functions
  faux_Skx_out <- sapply(seq(1:length(inp_ufunlist)),addconst)
  return(faux_Skx_out)
}