#' @include Transformation.R

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
#' @param after.complexity the complexity of the after functions
#' @return the corresponding new \code{Transformation} object
#' @seealso \code{\link{Transformation.andThen1}}
#' @export Transformation.andThen2
#' @importFrom functionComposeR function.compose
#' @importFrom methods validObject
#' @importFrom utilizeR function.args
Transformation.andThen2 <- function(before, after.forward, after.backward, after.complexity = 1L) {
  # Check the after.forward function
  after.forward <- force(after.forward);
  if(is.null(after.forward) ||
     (!(is.function(after.forward)))) {
    stop("after.forward function must be defined.");
  }
  if(!(identical(function.args(after.forward), c("x")))) {
    stop("after.forward function must have at exactly argument named 'x'.");
  }

  # Check the after.backward function
  after.backward <- force(after.backward);
  if(is.null(after.backward) ||
     (!(is.function(after.backward)))) {
    stop("after.backward function must be defined.");
  }
  if(!(identical(function.args(after.backward), c("x")))) {
    stop("after.backward function must have at exactly argument named 'x'.");
  }

  # check complexity
  if((!(is.integer(after.complexity))) || (after.complexity < 0L)) {
    stop("after.complexity must be positive integer or 0L.")
  }

  # we do a lot of forcing of variable values to try to ensure that
  # this works also in loops.
  before <- force(before);
  # validate object
  validObject(before);

  # Can we skip the composition?
  if(identical(after.forward, identity) &&
     identical(after.backward, identity) ) {
    return(before)
  }

  # Can we skip the composition II?
  if (identical(before@forward, identity) &&
      identical(before@backward, identity)) {
    result <- Transformation.new(forward = after.forward, backward = after.backward,
                                                  complexity = after.complexity);
  } else {
    result <- Transformation.new(
                forward = function.compose(before@forward, after.forward),
                backward = function.compose(after.backward, before@backward),
                complexity = (after.complexity + before@complexity));
  }

  result <- force(result);
  result@forward <- force(result@forward);
  result@backward <- force(result@backward);
  result@complexity <- force(result@complexity);
  validObject(result);
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
#' @importFrom methods validObject is
Transformation.andThen1 <- function(before, after) {
  # Some initial type checks
  if (missing(before) || is.null(before) ||
      (!(is(before, "Transformation")))) {
    stop("'before' transformation is null or missing.");
  }
  if (missing(after) || is.null(after) ||
      (!(is(after, "Transformation")))) {
    stop("'after' transformation is null or missing.");
  }

  # bind before functions
  before <- force(before);
  validObject(before);

  # bind after functions
  after <- force(after);
  validObject(after);

  result <- Transformation.andThen2(before,
                                    after.forward = after@forward,
                                    after.backward = after@backward,
                                    after.complexity = after@complexity);
  result <- force(result);
  result@forward <- force(result@forward);
  result@backward <- force(result@backward);
  result@complexity <- force(result@complexity);
  return(result);
}
