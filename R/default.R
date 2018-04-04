#' @include normalize.R
#' @include logScale.R
#' @include applyAll.R

# The default transformations
.Transformation.default <- c(Transformation.normalize,
#                            Transformation.normalizeNegated,
#                            Transformation.logNegated,
                             Transformation.log)


#' @title Apply All Default Transformations
#' @description Apply all default transformations to a dataset.
#' @param data the data
#' @param addIdentity should an identity transformation result be created as
#'   well? (by default \code{TRUE})
#' @return a list with the transformation results, or \code{NULL} if no
#'   transformation succeeded
#' @seealso \code{\link{Transformation.applyAll}}
#' @export Transformation.applyDefault
Transformation.applyDefault <- function(data, addIdentity=TRUE) {
  return(Transformation.applyAll(data=data,
                                 transformations=.Transformation.default,
                                 addIdentity=addIdentity));
}
