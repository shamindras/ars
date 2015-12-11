% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/faux_hPrimex.R
\name{faux_hPrimex}
\alias{faux_hPrimex}
\title{Helper function to get first derivative of h(x)}
\usage{
faux_hPrimex(inp_gfun, inp_xvec)
}
\arguments{
\item{inp_gfun}{A function of x which the user wants to generate samples from.
This function is used to calculate h(x)=ln(g(x)).}

\item{inp_xvec}{A number indicates the x-axis of the points}
}
\value{
the first derivative of h(x) at the point x
}
\description{
Helper function to get first derivative of h(x)
}
