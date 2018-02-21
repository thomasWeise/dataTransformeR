#' @include TransformedData2D.R
#' @include identity.R


#' @title Instantiate a new \code{\link{TransformedData2D}} Object
#' @description Always use this function for instantiating
#'   \code{\link{TransformedData2D}}
#' @param x the x data vector
#' @param y the y data vector
#' @return a new instance of \code{\link{TransformedData2D}}
#' @export Transformation.identity2D
Transformation.identity2D <- function(x, y) {
  TransformedData2D.new(dataTransformeR::Transformation.identity(x),
                        dataTransformeR::Transformation.identity(y));
}
