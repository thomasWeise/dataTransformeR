#' @include TransformedData2D.R
#' @include applyAll.R
#' @include identity.R

#' @title Apply All the Provided Transformations to a Two-Dimensional Dataset
#' @description Provide all the provided transformation functions to the \code{x} and \code{y} data.
#' @param x the \code{x} data
#' @param y the \code{y} data
#' @param x.transformations the transformations to be applied to the \code{x}
#'   data
#' @param y.transformations the transformations to be applied to the \code{y}
#'   data, by default the same as \code{x.transformations}
#' @param x.addIdentity should an identity transformation result be created as
#'   well for \code{x}? (\code{FALSE} by default)
#' @param y.addIdentity should an identity transformation result be created as
#'   well for \code{y}? (by default, the same as \code{x.addIdentity})
#' @export Transformation.applyAll2D
Transformation.applyAll2D <- function(x, y, x.transformations, y.transformations=x.transformations,
                                      x.addIdentity=FALSE, y.addIdentity=x.addIdentity) {
  x.result <- Transformation.applyAll(data=x,
                                      transformations=x.transformations,
                                      addIdentity=x.addIdentity);
  if(is.null(x.result)) { return(NULL); }

  y.result <- Transformation.applyAll(data=y,
                                      transformations=y.transformations,
                                      addIdentity=y.addIdentity);
  if(is.null(y.result)) { return(NULL); }

  result <- unlist(lapply(X=x.result, FUN=function(data.x) {
    return(unlist(lapply(X=y.result, FUN=function(data.y) {
      return(TransformedData2D.new(data.x, data.y));
    })))
  }));
  result <- force(result);
  if(is.null(result) || (length(result) <= 0)) { return(NULL); }

  return(result);
}
