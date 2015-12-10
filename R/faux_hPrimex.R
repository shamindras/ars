#' Helper function to get first derivative of h(x)
#' 
#' @param g A function of x which the user wants to generate samples from. 
#' This function is used to calculate h(x)=ln(g(x)).
#' @param x A number indicates the x-axis of the points 
#' @return the first derivative of h(x) at the point x 
#' @export

# Helper function to get h(x)

faux_hx <- function(g)
{
  #turn unevaluated expressions into character strings 
  g_str  <- deparse(body(g))
  #express and evaluate h(x)=log(g(x))
  h_x <- function(x) eval(parse(text = paste0("log(", g_str, ")")))
  return(h_x)
}

faux_hPrimex <- function(g,x)
{
  #set a relatively small number to calculate the derivative 
  h <- 0.0001
  h_x <- faux_hx(g)
  #the derivative is approximately (h(x+h)-h(x))/h 
  hPrimex <- (h_x(x+h)-h_x(x))/h
  return(hPrimex)
}
