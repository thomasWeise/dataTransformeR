% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/default.R
\name{Transformation.applyDefault}
\alias{Transformation.applyDefault}
\title{Apply All Default Transformations}
\usage{
Transformation.applyDefault(data, addIdentity = TRUE)
}
\arguments{
\item{data}{the data}

\item{addIdentity}{should an identity transformation result be created as
well? (by default \code{TRUE})}
}
\value{
a list with the transformation results, or \code{NULL} if no
  transformation succeeded
}
\description{
Apply all default transformations to a dataset.
}
\seealso{
\code{\link{Transformation.applyAll}}
}
