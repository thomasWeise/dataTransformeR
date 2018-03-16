#' @include Transformation.R
#' @include makePositive.R
#' @include apply.R

.Transformation.log <- Transformation.new(forward=function(x) log(x), backward=exp)

#' @title Log-Transform and then Normalize a Data Vector
#' @description A data vector is logarithmically scaled and then normalized,
#'   i.e., all of its components are logarithmically mapped into the interval
#'   \code{[0, 1]}.
#' @param data the data vector
#' @return a \code{\link{TransformedData}} instance corresponding to the
#'   logarithmic mapping of \code{data} into \code{[0, 1]}, or \code{NULL} if
#'   such a mapping is not possible, e.g., if \code{data} contained non-finite
#'   values
#' @seealso \code{\link{Transformation.apply}}
#' @export Transformation.log
#' @examples
#' data <- c(-1, 0, 2, 6, 14, 30)
#' Transformation.log(data)
#' # An object of class "TransformedData"
#' # Slot "transformation":
#' # An object of class "Transformation"
#' # Slot "forward":
#' # function (x)
#' # log(x + 2) * 0.288539008177793
#' # <environment: 0x3b5cc18>
#' #
#' # Slot "backward":
#' # function (x)
#' # exp(x = x * 3.46573590279973) - 2
#' # <environment: 0x3c19460>
#' #
#' # Slot "data":
#' # [1] 0.0 0.2 0.4 0.6 0.8 1.0
Transformation.log <- function(data) {
  return(Transformation.apply(data,
                              Transformation.andThen1(
                                Transformation.makePositive(data, FALSE),
                                .Transformation.log),
                              normalize=TRUE, negateNormalization=FALSE))
}

#' @title Log-Transform and then Normalize and Negate a Data Vector
#' @description A data vector is logarithmically scaled and then normalized and
#'   negated, i.e., all of its components are logarithmically mapped into the
#'   interval \code{[1, 0]}.
#' @param data the data vector
#' @return a \code{\link{TransformedData}} instance corresponding to the
#'   logarithmic mapping of \code{data} into \code{[1, 0]}, or \code{NULL} if
#'   such a mapping is not possible, e.g., if \code{data} contained non-finite
#'   values
#' @seealso \code{\link{Transformation.apply}}
#' @export Transformation.logNegated
Transformation.logNegated <- function(data) {
  return(Transformation.apply(data,
                              Transformation.andThen1(
                                Transformation.makePositive(data, FALSE),
                                .Transformation.log),
                              normalize=TRUE, negateNormalization=TRUE))
}
