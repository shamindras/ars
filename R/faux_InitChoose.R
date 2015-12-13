#' Helper function to choose two starting points
#'
#' @param inp_gfun A function user wants to generate samples from. This function
#' is used to calculate \eqn{h(x)=\log(g(x))}
#' @param inp_Dvec A vector with two elements indicates the support domain of
#'     the sample generation.
#' @param inp_Initnumsampvec An even integer determining the number of points to
#'     initially sample - should be even
#' @return two starting points.
#' @export
faux_InitChoose <- function(inp_gfun, inp_Dvec, inp_Initnumsampvec = 2){

    # Input validation with Error handling
    if(inp_Initnumsampvec %% 2 != 0 | inp_Initnumsampvec < 0){
        stop("inp_Initnumsampvec must be an even positive integer")
    }
    # Check that the user enters a support with exactly 2 elements
    if(length(inp_Dvec) != 2 | !is.numeric(inp_Dvec)){
        stop("inp_Dvec must be 2 numeric elements")
    }
    # Check that the first element of the inp_Dvec is less than the second 
    # element
    if(inp_Dvec[1] >= inp_Dvec[2]){
        stop("First element of inp_Dvec support interval must be greater 
            than the second element")
    }

    # Check that the inp_gfun is a valid R function
    if(!is.function(inp_gfun)){
        stop("inp_gfun must be a valid R function")
    }    
  
    # inp_Dvec must be in ascending order
    inp_Dvec <- sort(inp_Dvec)

    # User specified min/ max support
    minvec_support   <- inp_Dvec[1]
    maxvec_support   <- inp_Dvec[2]

    # Based on the support function, determine the type of bounds specified
    # e.g. (-Inf, Inf) then = "negInf_posInf"
    # e.g. (-Inf, 10) then  = "negInf_posBnd"
    # e.g. (-10, Inf) then =  "negBnd_posInf"
    # e.g. (-13, 55) then =   "negBnd_posBnd"
    if(is.infinite(minvec_support) & is.infinite(maxvec_support)){

        support_classify <- "negInf_posInf"
        # Set a reasonable optimum interval
        optim_intervalvec   <- c(def_faux_InitChoose_optimmin
                              , def_faux_InitChoose_optimmax)
        # Solve for the mode of the density i.e. where the max value occurs
        xvec_mode <- faux_findmode(optim_intervalvec = optim_intervalvec
                                   , inp_gfun = inp_gfun)$faux_findmode_par
    } else if (is.infinite(minvec_support) & !is.infinite(maxvec_support)){
        support_classify <- "negInf_posBnd"
        # Set a reasonable optimum interval
        optim_intervalvec   <- c(def_faux_InitChoose_optimmin
                              , min(def_faux_InitChoose_optimmax, maxvec_support))
        # Solve for the mode of the density i.e. where the max value occurs
        xvec_mode <- faux_findmode(optim_intervalvec = optim_intervalvec
                                   , inp_gfun = inp_gfun)$faux_findmode_par
    } else if (!is.infinite(minvec_support) & is.infinite(maxvec_support)){
        support_classify <- "negBnd_posInf"
        # Set a reasonable optimum interval
        optim_intervalvec   <- c(max(def_faux_InitChoose_optimmin, minvec_support)
                              , def_faux_InitChoose_optimmax)
        # Solve for the mode of the density i.e. where the max value occurs
        xvec_mode <- faux_findmode(optim_intervalvec = optim_intervalvec
                                   , inp_gfun = inp_gfun)$faux_findmode_par
    } else {
        support_classify <- "negBnd_posBnd"
        # Set a reasonable optimum interval
        optim_intervalvec   <- c(max(def_faux_InitChoose_optimmin, minvec_support)
                              , min(def_faux_InitChoose_optimmax, maxvec_support))
        # Solve for the mode of the density i.e. where the max value occurs
        xvec_mode <- faux_findmode(optim_intervalvec = optim_intervalvec
                                   , inp_gfun = inp_gfun)$faux_findmode_par
    }

    # Now we just need to sample specified number of points from the function
    # on either side of the mode but bounded by the support

    # Initialise dummy vector to collect samples
    choose_sample_points <- numeric(length = 0)
    # The number of points to sample around mode
    num_sample_pts_mode  <- inp_Initnumsampvec/2

    # Create the upper and lower bounds for the sample intervals
    sample_interval <- c(max(xvec_mode - def_faux_InitChoose_xmodedist
                             , def_faux_InitChoose_negInf
                             , minvec_support)
                         , min(xvec_mode + def_faux_InitChoose_xmodedist
                               , def_faux_InitChoose_posInf
                               , maxvec_support))

    # Choose the sample points
    choose_sample_points <- c(runif(n = num_sample_pts_mode
                                    , min = sample_interval[1]
                                    , max = xvec_mode)
                              , runif(n = num_sample_pts_mode
                                      , min = xvec_mode
                                      , max = sample_interval[2]))
    final_choose_sample_points <- sort(unique(choose_sample_points))

        # Return the output as required
    faux_InitChoose_out <- list("init_sample_points"    = final_choose_sample_points
                                , "num_sample_pts_mode" = num_sample_pts_mode
                                , "support_classify"    = support_classify
                                , "optim_intervalvec"   = optim_intervalvec
                                , "sample_interval"     = sample_interval
                                , "mode"                = xvec_mode
                                , "support"             = inp_Dvec
                                )
}