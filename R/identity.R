#' @include Transformation.R
#' @include TransformedData.R

#' @title Create an Identity \code{\link{TransformedData}} Instance
#' @description The resulting data will be the original data, i.e., an identity
#'   transformation.
#' @param data the data
#' @return the identity transformed data
#' @seealso \code{\link{TransformedData.new}}
#' @export Transformation.identity
#' @examples
#' Transformation.identity(c(1,2,3))
#' # An object of class "TransformedData"
#' # Slot "transformation":
#' #   An object of class "Transformation"
#' # Slot "forward":
#' #   function (x)
#' #     x
#' # <bytecode: 0x3fe24f0>
#' #   <environment: namespace:base>
#' #
#' #   Slot "backward":
#' #   function (x)
#' #     x
#' # <bytecode: 0x3fe25d0>
#' #   <environment: namespace:base>
#' #
#' #   Slot "complexity":
#' #   [1] 0
#' #
#' # Slot "data":
#' #   [1] 1 2 3
Transformation.identity <- function(data) {
  return(TransformedData.new(.Transformation.identity, data));
}
