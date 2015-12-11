#' Helper function to sample a value x*from \eqn{s_{k}(x)}
#' 
#' @param inp_uintervallist A list of intervals between z points 
#' @param inp_sfunlist A list of functions which form the \eqn{s_{k}(x)}
#'  function
#' @return an x value sampled from the function \eqn{s_{k}(x)}
#' @export
faux_SampleSkx <- function(inp_uintervallist, inp_sfunlist){
  
  # check that inp_uintervallist is a list
  if(!(class(inp_uintervallist))=="list"){
    stop("inp_uintervallist must be a list")
  }
  
  # check that inp_sfunlist is a list
  if(!(class(inp_sfunlist))=="list"){
    stop("inp_sfunlist must be a list")
  }
  
  # check that each element of inp_uintervallist is a 2-dim vector
  if(!(do.call(sum,lapply(inp_uintervallist,length)))==
     2*length(inp_uintervallist)){
    stop("Intervals in inp_uintervallist must be two dimensional vectors")
  }
  
  # check that every element of inp_sfunlist is a function
  if(!all.equal(lapply(inp_sfunlist,class),
                as.list(rep("function",length(inp_sfunlist))))){
    stop("inp_sfunlist must be a list of functions")
  }
  
  # function which will integrate each function in inp_sfunlist
  h <- function(i) integrate(inp_sfunlist[[i]], inp_uintervallist[[i]][1],
                             inp_uintervallist[[i]][2])[[1]]
  
  # vector of the area under each piecewise function in skx
  areas <- sapply(seq(1:length(inp_sfunlist)),h)
  
  # cumulative areas
  areasC <- cumsum(areas)
  
  # generate a random uniform
  U <- runif(1)
  
  # find in which interval the random uniform falls
  const <- U > areasC
  const <- which(!const)[1]
  
  # use the inverse cdf method to sample a point x from skx
  cdf <- function(z) sum(areas[c(1:(const-1))])+integrate(inp_sfunlist[[const]],
                                                          inp_uintervallist[[const]][1],z)[[1]]
  
  # use the uniroot function to solve for x
  rootfun <- function(z,u) cdf(z)-u
  r <- uniroot(rootfun,interval=c(-5,5),extendInt="yes",u=U)$root
  return(faux_SampleSkx_out=r)
}
