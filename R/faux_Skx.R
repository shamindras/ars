#' Helper function to create piecewise function {s_k(x)}
#' @param inp_uintervallist A list of intervals between z values, as in the output
#' from uInterval.
#' @param inp_ufunlist A list of functions, the output from uFun.
#' @return the function $s_{k}(x)$ for all x in the domain of g, in list format,
#' where each element of the list is one element of the piecewise function.
#' @export
faux_Skx <- function(inp_uintervallist, inp_ufunlist) {
    addexp <- function(i){
        str  <- deparse(body(inp_ufunlist[[i]]))
        h <- function(x) eval(parse(text = paste0("exp(", str, ")")))
        return(h)
    }
    exps <- sapply(seq(1:length(inp_ufunlist)),addexp)
    int <- function(i) integrate(exps[[i]], inp_uintervallist[[i]][1]
                                 , inp_uintervallist[[i]][2])[[1]]
    constant <- sum(sapply(seq(1:length(inp_ufunlist)),int))
    addconst <- function(i){
        str  <- deparse(body(inp_ufunlist[[i]]))
        h <- function(x) eval(parse(text = paste0("exp(", str, ")/",constant)))
        return(h)
    }
    faux_Skx_out <- sapply(seq(1:length(inp_ufunlist)),addconst)
    return(faux_Skx_out)
}