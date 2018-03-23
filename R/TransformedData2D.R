#' @include TransformedData.R

#' @title A Dataset Consisting of \code{x} and \code{y} Data
#' @description This class is the baseline for fitting models to \code{x}
#' and \code{y} data. It can encapsulate the data as well as
#' transformations that have been applied to it.
#' @slot x the x \code{\link{TransformedData}} instance
#' @slot y the y \code{\link{TransformedData}} instance
#' @exportClass TransformedData2D
#' @importFrom methods setClass is representation validObject
TransformedData2D <- methods::setClass(
  Class="TransformedData2D",
  representation=methods::representation(
    x="TransformedData",
    y="TransformedData"
  ),
  validity=function(object) {
    if(is.null(object@x) ||
       (!(is.object(object@x) &&
          methods::is(object@x, "TransformedData")))) {
      return ("The x data must be a proper instance of 'TransformedData'.");
    }
    methods::validObject(object@x);
    if(is.null(object@y) ||
       (!(is.object(object@y) &&
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
#' @importFrom methods new validObject
TransformedData2D.new <- function(x, y) {
  x <- force(x);
  y <- force(y);
  result <- methods::new("TransformedData2D", x=x, y=y);
  result <- force(result);
  result@x <- force(result@x);
  result@y <- force(result@y);
  methods::validObject(result);
  return(result);
}
