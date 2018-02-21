#' @include Transformation.R

#' @importFrom functionComposeR function.compose
#' @importFrom methods validObject is

#' @title Chain a Transformation given by Two Functions and another
#'   Transformation
#'
#' @description Create a new \code{Transformation} object resulting from the
#'   application of a new function \code{fwd} to the output of this object's
#'   \code{forward} function.
#' @param before the transformation to be applied first
#' @param after.forward the forward function to be applied to the result of the
#'   forward function of \code{before}
#' @param after.backward the corresponding backward function
#' @return the corresponding new \code{Transformation} object
#' @seealso \code{\link{Transformation.andThen1}}
#' @export Transformation.andThen2
Transformation.andThen2 <- function(before, after.forward, after.backward) {
  # Check the after.forward function
  after.forward <- base::force(after.forward);
  if(base::is.null(after.forward) || (!(base::is.function(after.forward)))) {
    return("after.forward function must be defined.");
  }
  if(base::is.primitive(after.forward)) {
    paramCount <- base::length(base::formals(base::args(after.forward)));
  } else {
    paramCount <- base::length(base::formals(after.forward));
  }
  if(paramCount != 1L) {
    return("after.forward function must have exactly one argument");
  }

  # Check the after.backward function
  after.backward <- base::force(after.backward);
  if(base::is.null(after.backward) || (!(base::is.function(after.backward)))) {
    return("after.backward function must be defined.");
  }
  if(base::is.primitive(after.backward)) {
    paramCount <- base::length(base::formals(base::args(after.backward)));
  } else {
    paramCount <- base::length(base::formals(after.backward));
  }
  if(paramCount != 1L) {
    return("after.backward function must have exactly one argument");
  }

  # we do a lot of forcing of variable values to try to ensure that
  # this works also in loops.
  before <- base::force(before);
  # validate object
  methods::validObject(before);

  # Can we skip the composition?
  if(base::identical(after.forward, base::identity) &&
     base::identical(after.backward, base::identity) ) {
    return(before)
  }

  # Can we skip the composition II?
  if (base::identical(before@forward, base::identity) &&
      base::identical(before@backward, base::identity)) {
    result <- dataTransformeR::Transformation.new(forward=after.forward, backward=after.backward);
  } else {
    result <- dataTransformeR::Transformation.new(
                forward=functionComposeR::function.compose(before@forward, after.forward),
                backward=functionComposeR::function.compose(after.backward, before@backward));
  }

  result <- base::force(result);
  result@forward <- base::force(result@forward);
  result@backward <- base::force(result@backward);
  methods::validObject(result);
  return (result)
}

#' @title Chain Two Transformations
#'
#' @description Create a new \code{\link{Transformation}} object resulting from
#'   the application of a \code{Transformation} to another one
#' @param before the first transformation
#' @param after the transformation to be applied after \code{before}
#' @return the corresponding \code{Transformation}
#' @seealso \code{\link{Transformation.andThen2}}
#' @export Transformation.andThen1
Transformation.andThen1 <- function(before, after) {
  # Some initial type checks
  if (missing(before) || is.null(before) ||
      (!(methods::is(before, "Transformation")))) {
    base::stop("'before' transformation is null or missing.");
  }
  if (missing(after) || is.null(after) ||
      (!(methods::is(after, "Transformation")))) {
    base::stop("'after' transformation is null or missing.");
  }

  # bind before functions
  before <- base::force(before);
  methods::validObject(before);

  # bind after functions
  after <- base::force(after);
  methods::validObject(after);

  result <- dataTransformeR::Transformation.andThen2(before,
                                    after.forward=after@forward,
                                    after.backward=after@backward);
  result <- base::force(result);
  result@forward <- base::force(result@forward);
  result@backward <- base::force(result@backward);
  return(result);
}
