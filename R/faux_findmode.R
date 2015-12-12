#' Helper function to find mode of a given univariate function \eqn{g(x)}
#'
#' @param optim_intervalvec A vector with two elements indicates the support domain of
#'     the sample generation.
#' @param inp_gfun A function of x which the user wants to generate samples from.
#'     This function is used to calculate \eqn{h(x)=\log(g(x))}.
#' @return two starting points.
#' @export
faux_findmode <- function(optim_intervalvec, inp_gfun){
    # Set a good interval for optimization

    guessedInterval = min(optim_intervalvec):max(optim_intervalvec)
    superStarSeed   = guessedInterval[which.max(inp_gfun(guessedInterval))]

    # Find the maximim
    faux_findmode <- optim(par=superStarSeed
                           , lower = min(optim_intervalvec)
                           , upper = max(optim_intervalvec)
                           , fn=function(y) -inp_gfun(y)
                           , method = "L-BFGS-B")

    faux_findmode_out <- list("min_int" = min(optim_intervalvec)
                              , "superStarSeed" = superStarSeed
                              , "faux_findmode_par" = faux_findmode$par)
    return(faux_findmode_out)
}
