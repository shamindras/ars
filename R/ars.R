#' Main function to carry out simulation 
#' 
#' @param n A number indicates how many accepted points user wants to generate. 
#' @param g A function user wants to generate samples from. 
#' @param D A vector with two elements indicates the domain of the sample 
#' generation.
#' @param k The number of inital x points to start the algorithm. Default is 100.
#' @return n accepted sampling points.
#' @references \url{https://stat.duke.edu/~cnk/Links/tangent.method.pdf}
#' @seealso \link[="https://stat.duke.edu/~cnk/Links/tangent.method.pdf"]{Gilks}
#' @keywords sample
#' @examples 
#' sample 1000 points from the standard normal distribution using adaptive
#' # rejection sample
#' 
#' set.seed(0)
#' dnorm1000 <- ars(1000,g=dnorm,D=c(-Inf,Inf))
#' hist(dnorm1000, breaks=30, main="1000 points sampled from N(0,1)")
#' 
#' # sample 500 point from the chisquare distribution with df=5
#' 
#' set.seed(123)
#' dchisq500 <- ars(500,g=function(x) dchisq(x,df=5), D=c(0,Inf))
#' hist(dchisq500, breaks=30)
#' 
#' # sample 1000 points from the exponential distribution
#' 
#' set.seed(0)
#' dexp1000 <- ars(1000,function(x) exp(-x), c(0,Inf))
#' hist(dexp1000,breaks=30)
#' 
#' @export
ars <- function(n,g,D,k=100){
  
  # get the initial starting x points where the tangents will be drawn
  xvec <- faux_InitChoose(inp_gfun=g,inp_Dvec=D,inp_Initnumsampvec=k)$init_sample_points
  
  # get log(g(x))
  hx <- faux_hx(inp_gfun=g)
  
  # get the points where the tangents intersect
  zvec <- faux_Zj(inp_xvec=xvec,inp_gfun=g,inp_Dvec=D)
  
  # get the intervals in between the z points
  uints <- faux_uInterval(inp_z=zvec)
  
  # get the upper bound function
  uk <- faux_Ukx(inp_xvec = xvec,inp_gfun = g)
  
  # get the lower bound function
  lk <- faux_Lkx(inp_xvec = xvec,inp_gfun = g)
  
  # get the normalized upper bound function, sk(x)
  sk <- faux_Skx(inp_uintervallist = uints,inp_ufunlist = uk)
  
  # initialize a vector which will hold the final points sampled from g(x)
  sample <- c()
  
  # while the number of points sampled from g(x) is less than n
  while(length(sample)<n){
    
    # sample a point from sk(x)
    xstar <- faux_SampleSkx(inp_uintervallist=uints, inp_sfunlist=sk)
    
    # generate a random uniform(0,1)
    w <- runif(1)
    
    # figure out which interval xstar falls into
    const  <- which(sapply(seq(1:length(uints)),function(i) xstar >=
                             uints[[i]][1] && xstar <= uints[[i]][2]))
    
    # if xstar passes the squeezing test, add it to the sample vector
    if(w <= exp(lk[[const]](xstar)-uk[[const]](xstar))){
      sample <- c(sample,xstar)} else if
    
    # if xstar was rejected in the squeeze test, try the rejection test
    (w <= exp(hx(xstar)-uk[[const]](xstar))){
      # if it passes the rejection test add it to the sample vector
      sample <- c(sample,xstar)
      
      # since xstar failed the squeeze test, add a tangent line at xstar
      # and update all of the intervals and functions
      if(!(is.na(faux_hPrimex(inp_gfun = g,inp_xvec = xstar)))){
        xvec <- sort(c(xvec,xstar))
        zvec <- sort(faux_Zj(inp_xvec = xvec,inp_gfun = g,inp_Dvec = D))
        uints <- faux_uInterval(inp_z=zvec)
        uk <- faux_Ukx(inp_xvec = xvec,inp_gfun = g)
        lk <- faux_Lkx(inp_xvec = xvec,inp_gfun = g)
        sk <- faux_Skx(inp_uintervallist = uints,inp_ufunlist = uk)
      }
    }
  }
  return(sample) # return a vector of lenth n of the points sampled from g(x)
}
