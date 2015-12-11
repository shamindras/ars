#' Helper function to choose two starting points
#'
#' @param inp_gfun A function user wants to generate samples from. This function is
#'     used to calculate h(x)=ln(g(x))
#' @param inp_Dvec A vector with two elements indicates the support domain of
#'     the sample generation.
#' @param inp_Initnumsampvec An even integer determining the number of points to
#'     initially sample - should be even
#' @return two starting points.
#' @export
faux_InitChoose <- function(inp_gfun, inp_Dvec, inp_Initnumsampvec = 2){

    if(inp_Initnumsampvec %% 2 != 0 | inp_Initnumsampvec < 0){
        stop("inp_Initnumsampvec must be an even positive integer")
    }
    # Input validation with Error handling
    if(length(inp_Dvec) != 2 | !is.numeric(inp_Dvec)){
        stop("inp_Dvec must be 2 numeric elements")
    }
    if(length(inp_Dvec) == 2 & is.numeric(inp_Dvec) & (inp_Dvec[1] >= inp_Dvec[2])){
        stop("inp_Dvec must be 2 numeric elements, first element smaller than
             the second element")
    }
    if(!is.function(inp_gfun)){
        stop("inp_gfun must be a valid R function")
    }

    # User specified min/ max support
    minvec_support   <- inp_Dvec[1]
    maxvec_support   <- inp_Dvec[2]

    # First get the gradient of the function - used for the mode
    gradfun <- function(x){grad(inp_gfun, x)}

    # Based on the support function, determine the type of bounds specified
    # e.g. (-Inf, Inf) then = "negInf_posInf"
    # e.g. (-Inf, 10) then  = "negInf_posBnd"
    # e.g. (-10, Inf) then =  "negBnd_posInf"
    # e.g. (-13, 55) then =   "negBnd_posBnd"
    if(is.infinite(minvec_support) & is.infinite(maxvec_support)){
        support_classify <- "negInf_posInf"
        # Solve for the mode of the density i.e. where the max value occurs
        xvec_mode <- optimize(f = inp_gfun,interval = c(def_faux_InitChoose_minLB
                                               , def_faux_InitChoose_maxUB)
                              , maximum=TRUE)$maximum
    } else if (is.infinite(minvec_support) & !is.infinite(maxvec_support)){
        support_classify <- "negInf_posBnd"
        # Solve for the mode of the density i.e. where the max value occurs
        xvec_mode <- optimize(f = inp_gfun,interval = c(def_faux_InitChoose_minLB
                                               , maxvec_support)
                              , maximum=TRUE)$maximum
    } else if (!is.infinite(minvec_support) & is.infinite(maxvec_support)){
        support_classify <- "negBnd_posInf"
        # Solve for the mode of the density i.e. where the max value occurs
        xvec_mode <- optimize(f = inp_gfun,interval = c(minvec_support
                                               , def_faux_InitChoose_maxUB)
                              , maximum=TRUE)$maximum
    } else {
        support_classify <- "negBnd_posBnd"
        # Solve for the mode of the density i.e. where the max value occurs
        xvec_mode <- optimize(f = inp_gfun,interval = c(minvec_support
                                               , maxvec_support)
                              , maximum=TRUE)$maximum
    }

    # Now we just need to sample specified number of points from the function
    # on either side of the mode but bounded by the support

    # Initialise dummy vector to collect samples
    choose_sample_points <- numeric(length = 0)
    # The number of points to sample around mode
    num_sample_pts_mode  <- inp_Initnumsampvec/2

    if(support_classify == "negInf_posInf"){
        choose_sample_points <- c(runif(n = num_sample_pts_mode
                                      , min = def_faux_InitChoose_minLB
                                      , max = xvec_mode)
                                  , runif(n = num_sample_pts_mode
                                          , min = xvec_mode
                                          , max = def_faux_InitChoose_maxUB))
    } else if (support_classify == "negInf_posBnd"){
        choose_sample_points <- c(runif(n = num_sample_pts_mode
                                        , min = def_faux_InitChoose_minLB
                                        , max = xvec_mode)
                                  , runif(n = num_sample_pts_mode
                                          , min = xvec_mode
                                          , max = maxvec_support))
    } else if (support_classify == "negBnd_posInf"){
        choose_sample_points <- c(runif(n = num_sample_pts_mode
                                        , min = minvec_support
                                        , max = xvec_mode)
                                  , runif(n = num_sample_pts_mode
                                          , min = xvec_mode
                                          , max = def_faux_InitChoose_maxUB))
    } else {
        choose_sample_points <- c(runif(n = num_sample_pts_mode
                                        , min = minvec_support
                                        , max = xvec_mode)
                                  , runif(n = num_sample_pts_mode
                                          , min = xvec_mode
                                          , max = maxvec_support))
    }

    # Return the output as required
    faux_InitChoose_out <- list("init_sample_points" = choose_sample_points
                                , "mode"              = xvec_mode
                                , "support"           = inp_Dvec
                                )
}