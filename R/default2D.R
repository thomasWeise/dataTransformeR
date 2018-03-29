#' @include normalize.R
#' @include logScale.R
#' @include applyAll2D.R
#' @include default.R

#' @title Apply All Default Transformations to a Two-Dimensional Dataset
#' @description Apply all default transformations to a dataset.
#' @param x the \code{x} data
#' @param y the \code{y} data
#' @param addIdentity should an identity transformation result be created as
#'   well? (by default \code{TRUE})
#' @return a list with the transformation results, or \code{NULL} if no
#'   transformation succeeded
#' @seealso \code{\link{Transformation.applyAll2D}}
#' @export Transformation.applyDefault2D
Transformation.applyDefault2D <- function(x, y, addIdentity=TRUE) {
  return(Transformation.applyAll2D(x=x, y=y,
                                  x.transformations=.Transformation.default,
                                  y.transformations=.Transformation.default,
                                  x.addIdentity=addIdentity,
                                  y.addIdentity=addIdentity));
}
