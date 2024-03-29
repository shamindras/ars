% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/faux_InitChoose.R
\name{faux_InitChoose}
\alias{faux_InitChoose}
\title{Helper function to choose two starting points}
\usage{
faux_InitChoose(inp_gfun, inp_Dvec, inp_Initnumsampvec = 2)
}
\arguments{
\item{inp_gfun}{A function user wants to generate samples from. This function
is used to calculate \eqn{h(x)=\log(g(x))}{h(x) = ln(g(x))}}

\item{inp_Dvec}{A vector with two elements indicates the support domain of
the sample generation.}

\item{inp_Initnumsampvec}{An even integer determining the number of points to
initially sample - should be even}
}
\value{
A list with 7 elements.
\item{init_sample_points}{}
\item{num_sample_pts_mode}{}
\item{support_classify}{Based on the support function, determine the type of 
   bounds specified:
   e.g. (-Inf, Inf) then = "negInf_posInf"
   e.g. (-Inf, 10) then  = "negInf_posBnd"
   e.g. (-10, Inf) then =  "negBnd_posInf"
   e.g. (-13, 55) then =   "negBnd_posBnd"}
\item{mode}{This is a single element vector returning the mode of the log of 
   the function \code{inp_gfun}}.
\item{support}{The support of the function \code{inp_gfun}.
Equivalent to \code{inp_Dvec} but in ascending order.}
}
\description{
Helper function to choose two starting points
}

