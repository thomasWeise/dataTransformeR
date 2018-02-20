#' @include Transformation.R
#' @include TransformedData.R

#' @title Create an Identity \code{\link{TransformedData}} Instance
#' @description The resulting data will be the original data, i.e., an identity
#'   transformation.
#' @param data the data
#' @return the identity transformed data
#' @seealso \code{\link{TransformedData.new}}
#' @export Transformation.identity
Transformation.identity <- function(data) {
  return(TransformedData.new(.Transformation.identity, data));
}
