#' @include Transformation.R
#' @importFrom methods new is representation validObject

#' @title The Result of a Single Transformation Step
#' @description A class for presenting the results of the transformation of a
#'   data column based on a \code{\link{Transformation}}
#' @slot transformation the \code{\link{Transformation}} between the original data and the transformed
#'   data
#' @slot data the transformed data
#' @exportClass TransformedData
TransformedData <- methods::setClass(
  Class="TransformedData",
  representation=methods::representation(
    transformation="Transformation",
    data="numeric"
  ),
  validity=function(object) {
    if(base::is.null(object@transformation) ||
       (!(base::is.object(object@transformation)))) {
      return("Transformation cannot be null.");
    }
    if(!(methods::is(object@transformation, "Transformation"))) {
      return ("The transformation must be an instance of 'Transformation'.");
    }
    methods::validObject(object@transformation);
    if(base::is.null(object@data) || (!(base::is.vector(object@data))) ||
       (base::length(object@data) <= 0)) {
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
TransformedData.new <- function(transformation, data) {
  transformation <- base::force(transformation);
  data <- base::force(data);
  result <- methods::new("TransformedData", transformation=transformation, data=data);
  result <- base::force(result);
  result@transformation <- base::force(result@transformation);
  result@data <- base::force(result@data);
  return(result);
}