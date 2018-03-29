#' @include Transformation.R

#' @title The Result of a Single Transformation Step
#' @description A class for presenting the results of the transformation of a
#'   data column based on a \code{\link{Transformation}}
#' @slot transformation the \code{\link{Transformation}} between the original data and the transformed
#'   data
#' @slot data the transformed data
#' @exportClass TransformedData
#' @importFrom methods setClass is representation validObject
TransformedData <- setClass(
  Class="TransformedData",
  representation=representation(
    transformation="Transformation",
    data="numeric"
  ),
  validity=function(object) {
    if(is.null(object@transformation) ||
       (!(is.object(object@transformation)))) {
      return("Transformation cannot be null.");
    }
    if(!(is(object@transformation, "Transformation"))) {
      return ("The transformation must be an instance of 'Transformation'.");
    }
    validObject(object@transformation);
    if(is.null(object@data) || (!(is.vector(object@data))) ||
       (length(object@data) <= 0L)) {
      return("Data must be a valid, no-empty vector.");
    }
    return(TRUE);
  }
)

#' @title Instantiate a new \code{\link{TransformedData}} Object
#' @description Always use this function for instantiating
#'   \code{\link{TransformedData}}
#' @param transformation the transformation
#' @param data the transformed data
#' @return a new instance of \code{\link{TransformedData}}
#' @export TransformedData.new
#' @importFrom methods new validObject
TransformedData.new <- function(transformation, data) {
  transformation <- force(transformation);
  data <- force(data);
  result <- new("TransformedData", transformation=transformation, data=data);
  result <- force(result);
  result@transformation <- force(result@transformation);
  result@data <- force(result@data);
  validObject(result);
  return(result);
}
