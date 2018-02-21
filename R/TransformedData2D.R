#' @include TransformedData.R

#' @importFrom methods new is representation validObject

#' @title A Dataset Consisting of \code{x} and \code{y} Data
#' @description This class is the baseline for fitting models to \code{x}
#' and \code{y} data. It can encapsulate the data as well as
#' transformations that have been applied to it.
#' @slot x the x \code{\link{TransformedData}} instance
#' @slot y the y \code{\link{TransformedData}} instance
#' @exportClass TransformedData2D
TransformedData2D <- methods::setClass(
  Class="TransformedData2D",
  representation=methods::representation(
    x="TransformedData",
    y="TransformedData"
  ),
  validity=function(object) {
    if(base::is.null(object@x) ||
       (!(base::is.object(object@x) &&
          methods::is(object@x, "TransformedData")))) {
      return ("The x data must be a proper instance of 'TransformedData'.");
    }
    methods::validObject(object@x);
    if(base::is.null(object@y) ||
       (!(base::is.object(object@y) &&
          methods::is(object@y, "TransformedData")))) {
      return ("The y data must be a proper instance of 'TransformedData'.");
    }
    methods::validObject(object@y);
    return(TRUE);
  }
)

#' @title Instantiate a new \code{\link{TransformedData2D}} Object
#' @description Always use this function for instantiating
#'   \code{\link{TransformedData2D}}
#' @param x the x \code{\link{TransformedData}} instance
#' @param y the y \code{\link{TransformedData}} instance
#' @return a new instance of \code{\link{TransformedData2D}}
#' @export TransformedData2D.new
TransformedData2D.new <- function(x, y) {
  x <- base::force(x);
  y <- base::force(y);
  result <- methods::new("TransformedData2D", x=x, y=y);
  result <- base::force(result);
  result@x <- base::force(result@x);
  result@y <- base::force(result@y);
  methods::validObject(result);
  return(result);
}