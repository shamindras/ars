% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/faux_Ukx.R
\name{faux_Ukx}
\alias{faux_Ukx}
\title{Helper function to get the upper bound linear function \eqn{u_{k}(x)}{uk(x)}}
\usage{
faux_Ukx(inp_xvec, inp_gfun)
}
\arguments{
\item{inp_xvec}{A vector of x values of all points}

\item{inp_gfun}{A function user wants to generate samples from. This function is
used to calculate \eqn{h(x)=\log(g(x))}{h(x) = ln(g(x))}}
}
\value{
A list of functions.
The length of the list is equal to the length of the input \code{inp_xvec}.
Each of the elements of the list is a piece of the piecewise function
\eqn{u_{k}(x)}{uk(x)}, which forms the upper hull of \eqn{h(x)}.
}
\description{
Helper function to get the upper bound linear function \eqn{u_{k}(x)}{uk(x)}
}

