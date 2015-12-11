#' Helper function to get h(x) = ln(g(x)).
#'
#' @param inp_gfun A function of x which the user wants to generate samples from.
#' This function is used to calculate h(x)=ln(g(x)).
#' @return the h(x)
#' @export
faux_hx <- function(inp_gfun){
  #inp_gfun must be a function
  if(!is.function(inp_gfun)){
    stop("inp_gfun must be a valid R function")
  }
  #turn unevaluated expressions into character strings
  g_str  <- deparse(body(inp_gfun))
  #express and evaluate h(x)=log(g(x))
  faux_hx_out <- function(x) eval(parse(text = paste0("log(", g_str, ")")))
  return(faux_hx_out)
}