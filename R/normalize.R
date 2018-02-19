#' @include Transformation.R
#' @include apply.R

#' @title Normalize a Data Vector
#' @description A data vector is normalized, i.e., all of its components are
#'   linearly mapped into the interval \code{[0, 1]}.
#' @param data the data vector
#' @return a \code{\link{TransformedData}} instance corresponding to the linear
#'   mapping of \code{data} into \code{[0, 1]}, or \code{NULL} if such a mapping
#'   is not possible, e.g., if \code{data} contained non-finite values
#' @seealso \code{\link{Transformation.apply}}
#' @export Transformation.normalize
Transformation.normalize <- function(data) {
  Transformation.apply(data, .Transformation.identity, normalize=TRUE, negateNormalization=FALSE)
}

#' @title Normalize a Negated Data Vector
#' @description A data vector is first negated and then normalized, i.e., all of
#' its components are linearly mapped into the interval \code{[0, 1]}.
#' @param data the data vector
#' @return a \code{\link{TransformedData}} instance corresponding to the linear
#'   negated mapping of \code{data} into \code{[0, 1]}, or \code{NULL} if such a
#'   mapping is not possible, e.g., if \code{data} contained non-finite values
#' @seealso \code{\link{Transformation.apply}}
#' @export Transformation.normalizeNegated
Transformation.normalizeNegated <- function(data) {
  Transformation.apply(data, .Transformation.identity, normalize=TRUE, negateNormalization=TRUE)
}