#'@title A Transformation, i.e. a Two-Way Mapping
#'
#'@description This class holds two mutually inverse functions for transforming
#'  data, i.e., allows us to transform data forward and backward. It should hold
#'  \code{b@backward(b@forward(x))==x} for a Transformation \code{b} and values
#'  \code{x} in the domain of the Transformation. At the same time,
#'  \code{b@forward(b@backward(y))==y} should hold for all \code{y} in the image
#'  of the Transformation.
#'
#'  A transformation is furthermore accompanied by a positive \code{complexity}.
#'  Only \code{\link{Transformation.identity}} has complexity \code{0L}. All other
#'  transformations should have a larger complexity. If we simply log-scale some
#'  data (e.g., via \code{\link{Transformation.log}}) by just applying the
#'  \code{log} function, this could have complexity \code{1L}. If we have a
#'  transformation involving \code{n} variables whose values we decide upon,
#'  then we should pick complexity \code{n+1L}: If we first move the data by
#'  3 units and then divide it by 2, i.e., apply something like \code{(x+3)/2},
#'  this transformation should have a complexity of \code{3} - we chose two
#'  values and applied them in a function.
#'
#' @slot forward the foward transformation function
#' @slot backward the inverse of the transformation function, i.e., the backwards
#'   transformation
#' @slot complexity a measure of the transformation complexity, e.g., one plus
#'                  the number of constants involved in the transformation - only
#'                  the identity transformation has complexity 0
#' @exportClass Transformation
#' @importFrom methods setClass representation prototype
#' @importFrom utilizeR function.args
#' @examples
#' new("Transformation", forward=sin, backward=asin, complexity=1L)
#' # An object of class "Transformation"
#' # Slot "forward":
#' # function (x)  .Primitive("sin")
#' #
#' # Slot "backward":
#' # function (x)  .Primitive("asin")
#' #
#' # Slot "complexity":
#' # [1] 1
Transformation <- setClass(
  Class = "Transformation",
  representation = representation(forward="function",
                                           backward="function",
                                           complexity="integer"),
  prototype = prototype(complexity = 1L),
  validity = function(object) {
    # check forward function
    if(is.null(object@forward) ||
       (!(is.function(object@forward)))) {
      return("Forward function must be defined.");
    }
    if(!(identical(function.args(object@forward), c("x")))) {
      return("Forward function must have at exactly one argument named 'x'.");
    }

    # check backward function
    if(is.null(object@backward) ||
       (!(is.function(object@backward)))) {
      return("Backward function must be defined.");
    }
    if(!(identical(function.args(object@backward), c("x")))) {
      return("Backward function must have at exactly one argument named 'x'.");
    }

    if(xor(identical(object@forward, identity),
           identical(object@backward, identity))) {
      stop("Either neither or both transformation functions can be the identity transformation.")
    }

    # check complexity
    if((!(is.integer(object@complexity))) ||
        (object@complexity < 0L) ||
        (length(object@complexity) != 1L)) {
      return("Transformation complexity must be a single positive integer or 0L.");
    }

    if((object@complexity <= 0L) &&
       (!(identical(object@forward, identity)))) {
      stop("Only identity transformation can have complexity 0L.")
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
#' @param complexity the complexity
#' @return the new \code{\link{Transformation}}
#' @export Transformation.new
#' @importFrom methods validObject new
#' @importFrom functionComposeR function.canonicalize
Transformation.new <- function(forward, backward, complexity = 1L) {
  if(!(is.primitive(forward))) {
    forward <- function.canonicalize(forward);
  }
  if(!(is.primitive(backward))) {
    backward <- function.canonicalize(backward);
  }
  result <- new("Transformation", forward=forward, backward=backward, complexity=complexity);
  result <- force(result);
  result@forward <- force(result@forward);
  result@backward <- force(result@backward);
  result@complexity <- force(result@complexity);
  validObject(result);
  return(result);
}

# The internal identity transformation constant
.Transformation.identity <- new("Transformation", forward = identity,
                                                           backward = identity,
                                                           complexity = 0L)
