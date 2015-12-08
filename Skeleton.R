#' Main function to carry out simulation 
#' 
#' @param n A number indicates how many accepted points user wants to generate. 
#' @param g A function user wants to generate samples from. 
#' @param D A vector with two elements indicates the domain of the sample 
#' generation.
#' @return n accepted sampling points. 
#' @export

ars <- function(n,g,D) { }

#' Helper function to choose two starting points 
#' 
#' @param g A function user wants to generate samples from. This function is 
#' used to calculate h(x)=ln(g(x)).
#' @param D A vector with two elements indicates the domain of the sample 
#' generation.
#' @return two starting points. 
#' @export

choose2 <- function(g,D) { }

#' Helper function to get first derivative of h(x)
#' 
#' @param g A function user wants to generate samples from. This function is 
#' used to calculate h(x)=ln(g(x)).
#' @param x A number indicates the x-axis of the points 
#' @return the first derivative of h(x) at the point x 
#' @export

hPrime <- function(g,x) { }

#' Helper function to get the intersection of tangents at $x_{j}$ and $x_{j+1}$
#' 
#' @param x A vector of x values of your points. The first element in the vector
#' has smaller x-coordinate. 
#' @param g A fucntion user wants to generate samples from. This function is 
#' used to calculate h(x)=ln(g(x))
#' @return intersection of tangents of $x_{j}$ and $x_{j+1}$ named $z_{j}$
#' @export

zFun <- function(x,g) { }

#' Helper function to get the upper bound linear function $u_{k}(x)$
#' 
#' @param x A vector of x values of all points
#' @param z A vector of z values of all points and we should be able to get 
#' the index of z
#' @param g A function user wants to generate samples from. This function is 
#' used to calculate h(x)=ln(g(x))
#' @return the upper bound linear function $u_{k}(x)$ for x in the interval 
#' [$z_{j-1}$,$z_{j}$]
#' @export

uFun <- function(x,z,g) { }

#' Helper function to get function $s_{k}(x)$
#' 
#' @param x A vector of x values of all points 
#' @param z A vector of z values of all points and we should be able to get 
#' the index of z
#' @param g A function user wants to generate samples from. This function is 
#' used to calculate h(x)=ln(g(x))
#' @return the function $s_{k}(x)$ for x in the interval [$z_{j-1}$,$z_{j}$]
#' @export

sFun <- function(x,z,g) { }
  
#' Helper function to sample a value x*from $s_{k}(x)$
#' 
#' @param x A vector of x values of all points 
#' @param z A vector of z values of all points and we should be able to get 
#' the index of z
#' @param g A function user wants to generate samples from. This function is 
#' used to calculate h(x)=ln(g(x))
#' @return a x value sampling from function $s_{k}(x)$
#' @export

sSample <- function(x,z,g) { }
  
#' Helper function to get the lower bound linear function $l_{k}(x)$
#' 
#' @param x A vector of x values of all points and we should be able to get the
#' index of x 
#' @param g A function user wants to generate samples from. This function is 
#' use to calculate h(x)=ln(g(x))
#' @return the lower bound linear function $l_{k}(x)$ for x in the interval 
#' [$x_{j}$,$x_{j+1}$]
#' @export

lFun <- function(x,g) { }

#' Helper function to check the log concavity 
#' @param f A function that requires the log concavity check 
#' @return boolean value TRUE or FALSE to indicate the log concavity 
#' @export

checkLC <- function(f) { }
  
#' Helper function to check if the point is accepted as one of the generating
#' samples 
#' @param x A vector of x values of all points 
#' @param z A vector of z values of all points and we should be able to get 
#' the index of z
#' @param g A function user wants to generate samples from. This function is 
#' used to calculate h(x)=ln(g(x))
#' @return A vector of boolean value TRUE or FALSE indicating whether it's 
#' accepted and the value of x*
#' @export

checkAccept <- function(x,z,g) { }

package.skeleton(name = "ars", list = c("ars", "choose2", "hPrime", "zFun",
                                        "uFun","sFun","sSample","lFun",
                                        "checkLC","checkAccept"))







