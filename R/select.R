#' @include TransformedData.R
#' @include TransformedData2D.R


#' @importFrom methods setClassUnion
setClassUnion(".dataTransformeR.vectorOrNULL", c("numeric","NULL"))

#' @title Creating a Sub-Selection of a Transformed Dataset Instance.
#' @description Implementations of this method must accept an instance of a
#'   transformed data-like object and an integer value and then return a
#'   sub-selection of that data.
#' @name TransformedData.select
#' @param data the data to sub-select
#' @param selection the selection (or \code{NULL} to select verything)
#' @return an instance of the same class as \code{data}, but only containing the
#'   elements listed in \code{selection}.
#' @importFrom methods setGeneric
#' @exportMethod TransformedData.select
#' @docType methods
#' @aliases TransformedData.select
#' @rdname TransformedData.select
setGeneric(
  name="TransformedData.select",
  def=function(data, selection) {
    standardGeneric("TransformedData.select")
  }
)

#' @title Select a Sub-Set of a \code{\link{TransformedData}} Instance
#' @description This method selects a subset of the provided instance of
#'   \code{\link{TransformedData}}
#' @param data the \code{\link{TransformedData}} instance
#' @param selection the selection, or \code{NULL} to select all
#' @return the corresponding subset
#' @export TransformedData.select1D
TransformedData.select1D <- function(data, selection) {
  if(is.null(selection)) {
    return(data);
  }
  return(TransformedData.new(data@transformation, data@data[selection]));
}

#' @name TransformedData.select
#' @aliases TransformedData.select,TransformedData,.dataTransformeR.vectorOrNULL-method
#' @rdname TransformedData.select
setMethod(
  f="TransformedData.select",
  signature=c("TransformedData", ".dataTransformeR.vectorOrNULL"),
  definition=TransformedData.select1D
)

#' @title Select a Sub-Set of a \code{\link{TransformedData2D}} Instance
#' @description This method selects a subset of the provided instance of
#'   \code{\link{TransformedData2D}}
#' @param data the \code{\link{TransformedData2D}} instance
#' @param selection the selection, or \code{NULL} to select all
#' @return the corresponding subset
#' @export TransformedData.select2D
TransformedData.select2D <- function(data, selection) {
  if(is.null(selection)) { return(data); }
  return(TransformedData2D.new(
    TransformedData.select1D(data@x, selection),
    TransformedData.select1D(data@y, selection)));
}

#' @aliases TransformedData.select,TransformedData2D,.dataTransformeR.vectorOrNULL-method
#' @rdname TransformedData.select
setMethod(
  f="TransformedData.select",
  signature=c("TransformedData2D", ".dataTransformeR.vectorOrNULL"),
  definition=TransformedData.select2D
)
