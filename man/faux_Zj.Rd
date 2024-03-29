% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/faux_Zj.R
\name{faux_Zj}
\alias{faux_Zj}
\title{Helper function to get the intersection of tangents at \eqn{x_{j}} and
\eqn{x_{j+1}}}
\usage{
faux_Zj(inp_xvec, inp_gfun, inp_Dvec)
}
\arguments{
\item{inp_xvec}{A vector of \eqn{x} values of your points. The vector is
ordered in an increasing order.}

\item{inp_gfun}{A fucntion user wants to generate samples from. This function 
is used to calculate \eqn{h(x)=\log(g(x))}{h(x) = ln(g(x))}}

\item{inp_Dvec}{A vector with 2 elements indicating the domain the function
\eqn{g}}
}
\value{
A numeric vector.
The elements of the vector are the intersection points
of \eqn{s_k(x)}{sk(x)}, the upper hull of the function \eqn{h(x)}.
}
\description{
Helper function to get the intersection of tangents at \eqn{x_{j}} and
\eqn{x_{j+1}}
}

