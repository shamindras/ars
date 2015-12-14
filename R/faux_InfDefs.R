# Define the various numerical Inf values for the faux functions used in ars

# 1. FAUX_INITCHOOSE
# 1a. Negative/ Positive Inf
# def_faux_InitChoose_negInf        <- -1e+16
# def_faux_InitChoose_posInf        <- 1e+16

# 1b. Maximum range to consider for optimization to calculate the mode (max) of g(x)
# def_faux_InitChoose_optimmin      <- -1e4
# def_faux_InitChoose_optimmax      <- 1e4
# def_faux_InitChoose_xmodedist     <- 30

# 2. FAUX_HPRIMEX
# 2a. Tolerance for calculating numerical derivatives in FAUX_HPRIMEX
# faux_hPrimex_tol                  <- 1e-06
# def_faux_hPrime_Perturb             <- 1e-04

# 3. FAUX_LKX
# def_faux_Lkx_negInf               <- -1e+16

# Load the Infinity definitions into the dataset
# devtools::use_data(def_faux_InitChoose_negInf     , overwrite = TRUE)
# devtools::use_data(def_faux_InitChoose_posInf     , overwrite = TRUE)
# devtools::use_data(def_faux_InitChoose_optimmin   , overwrite = TRUE)
# devtools::use_data(def_faux_InitChoose_optimmax   , overwrite = TRUE)
# devtools::use_data(def_faux_InitChoose_xmodedist  , overwrite = TRUE)
# devtools::use_data(faux_hPrimex_tol               , overwrite = TRUE)
# devtools::use_data(def_faux_Lkx_negInf            , overwrite = TRUE)
# devtools::use_data(def_faux_hPrime_Perturb        , overwrite = TRUE)
