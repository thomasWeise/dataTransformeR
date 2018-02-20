#' @importFrom methods setClass representation validObject
#' @importFrom functionComposeR function.canonicalize

#'@title A Transformation, i.e. a Two-Way Mapping
#'
#'@description This class holds two mutually inverse functions for transforming
#'  data, i.e., allows us to transform data forward and backward. It should hold
#'  \code{b@backward(b@forward(x))==x} for a Transformation \code{b} and values
#'  \code{x} in the domain of the Transformation. At the same time,
#'  \code{b@forward(b@backward(y))==y} should hold for all \code{y} in the image
#'  of the Transformation.
#'
#'@slot forward the foward transformation function
#'@slot backward the inverse of the transformation function, i.e., the backwards
#'  transformation
#' @exportClass Transformation
Transformation <- methods::setClass(
  Class = "Transformation",
  representation = methods::representation(forward="function",
                                           backward="function"),
  validity = function(object) {
    if(base::is.null(object@forward) ||
       (!(base::is.function(object@forward)))) {
      return("Forward function must be defined.");
    }
    if(base::is.primitive(object@forward)) {
      paramCount <- base::length(base::formals(base::args(object@forward)));
    } else {
      paramCount <- base::length(base::formals(object@forward));
    }
    if(paramCount != 1L) {
      return("Forward function must have at exactly argument");
    }

    if(base::is.null(object@backward) ||
       (!(base::is.function(object@backward)))) {
      return("Backward function must be defined.");
    }
    if(base::is.primitive(object@backward)) {
      paramCount <- base::length(base::formals(base::args(object@backward)));
    } else {
      paramCount <- base::length(base::formals(object@backward));
    }
    if(paramCount != 1L) {
      return("Backward function must have exactly one argument");
    }
    return(TRUE);
  }
)

#' @title Construct a new Transformation
#' @description Always use this method instead of the constructor to build a new
#'   instance of \code{\link{Transformation}}, as it will canonicalize the
#'   functions used and ensure that all promises are resolved before returning
#'   the result.
#' @param forward the forward mapping
#' @param backward the backward mapping
#' @return the new \code{\link{Transformation}}
#' @export Transformation.new
Transformation.new <- function(forward, backward) {
  if(!(base::is.primitive(forward))) {
    forward <- functionComposeR::function.canonicalize(forward);
  }
  if(!(base::is.primitive(backward))) {
    backward <- functionComposeR::function.canonicalize(backward);
  }
  result <- methods::new("Transformation", forward=forward, backward=backward);
  result <- base::force(result);
  result@forward <- base::force(result@forward);
  result@backward <- base::force(result@backward);
  methods::validObject(result);
  return(result);
}

# The internal identity transformation constant
.Transformation.identity <- methods::new("Transformation", forward=base::identity, backward=base::identity)