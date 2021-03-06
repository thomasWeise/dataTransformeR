% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Transformation.R
\docType{class}
\name{Transformation-class}
\alias{Transformation-class}
\alias{Transformation}
\title{A Transformation, i.e. a Two-Way Mapping}
\description{
This class holds two mutually inverse functions for transforming
 data, i.e., allows us to transform data forward and backward. It should hold
 \code{b@backward(b@forward(x))==x} for a Transformation \code{b} and values
 \code{x} in the domain of the Transformation. At the same time,
 \code{b@forward(b@backward(y))==y} should hold for all \code{y} in the image
 of the Transformation.

 A transformation is furthermore accompanied by a positive \code{complexity}.
 Only \code{\link{Transformation.identity}} has complexity \code{0L}. All other
 transformations should have a larger complexity. If we simply log-scale some
 data (e.g., via \code{\link{Transformation.log}}) by just applying the
 \code{log} function, this could have complexity \code{1L}. If we have a
 transformation involving \code{n} variables whose values we decide upon,
 then we should pick complexity \code{n+1L}: If we first move the data by
 3 units and then divide it by 2, i.e., apply something like \code{(x+3)/2},
 this transformation should have a complexity of \code{3} - we chose two
 values and applied them in a function.
}
\section{Slots}{

\describe{
\item{\code{forward}}{the foward transformation function}

\item{\code{backward}}{the inverse of the transformation function, i.e., the backwards
transformation}

\item{\code{complexity}}{a measure of the transformation complexity, e.g., one plus
the number of constants involved in the transformation - only
the identity transformation has complexity 0}
}}

\examples{
new("Transformation", forward=sin, backward=asin, complexity=1L)
# An object of class "Transformation"
# Slot "forward":
# function (x)  .Primitive("sin")
#
# Slot "backward":
# function (x)  .Primitive("asin")
#
# Slot "complexity":
# [1] 1
}
