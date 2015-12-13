#' Main function to carry out simulation 
#' 
#' @param n A number indicates how many accepted points user wants to generate. 
#' @param g A function user wants to generate samples from. 
#' @param D A vector with two elements indicates the domain of the sample 
#' generation.
#' @return A numeric vector of length \code{n}, each element of which is
#' a value sampled from \eqn{g}. 
#' @export
ars <- function(n,g,D){
  xvec <- faux_InitChoose(inp_gfun=g,inp_Dvec=D)
  
  #   if (length(xvec)==2){
  #     pnts <- c(xvec[1],(xvec[1]+xvec[2])/2,xvec[2])
  #     if (faux_CheckLogConcavity(pnts,g)==FALSE){
  #       stop("g is not log concave")}
  #   } else 
  #     {l <- list()
  #     length(l) <- length(xvec)-2
  #     for (i in 1:(length(xvec)-2)){
  #       l[[i]] <- xvec[i:(i+2)]
  #     }
  #     l <- lapply(l,faux_CheckLogConcavity,g)
  #     if (!(do.call(sum,l)==length(l))){
  #       stop("g is not log concave")
  #     }
  #     }  else {
  
  hx <- faux_hx(g)
  zvec <- faux_Zj(xvec,g,D)
  uints <- faux_uInterval(zvec)
  uk <- faux_Ukx(xvec,g)
  lk <- faux_Lkx(xvec,g)
  sk <- faux_Skx(uints,uk)
  sample <- c()
  
  while(length(sample)<n){
    
    xstar <- faux_SampleSkx(inp_uintervallist=uints, inp_sfunlist=sk)
    w <- runif(1)
    const  <- which(sapply(seq(1:length(uints)),function(i) xstar >=
                             uints[[i]][1] && xstar <= uints[[i]][2]))
    if(w <= exp(lk[[const]](xstar)-uk[[const]](xstar))){
      sample <- c(sample,xstar)} else if
    (w <= exp(hx(xstar)-uk[[const]](xstar))){
      sample <- c(sample,xstar)
      if(!(is.na(faux_hPrimex(g,xstar)))){
        xvec <- sort(c(xvec,xstar))
        zvec <- sort(faux_Zj(xvec,g,D))
        uints <- faux_uInterval(zvec)
        uk <- faux_Ukx(xvec,g)
        lk <- faux_Lkx(xvec,g)
        sk <- faux_Skx(uints,uk)
      }
    }
  }
  return(sample)
}
