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
Transformation.andThen2 <- function(before, after.forward, after.backward, after.complexity = 1L) {
  # Check the after.forward function
  after.forward <- base::force(after.forward);
  if(base::is.null(after.forward) ||
     (!(base::is.function(after.forward)))) {
    base::stop("after.forward function must be defined.");
  }
  if(base::is.primitive(after.forward)) {
    forward.args <- base::formals(base::args(after.forward));
  } else {
    forward.args <- base::formals(after.forward);
  }
  if((base::length(forward.args) != 1L) ||
     (!(base::identical(base::names(forward.args), base::c("x"))))) {
    base::stop("after.forward function must have at exactly argument named 'x'.");
  }

  # Check the after.backward function
  after.backward <- base::force(after.backward);
  if(base::is.null(after.backward) ||
     (!(base::is.function(after.backward)))) {
    base::stop("after.backward function must be defined.");
  }
  if(base::is.primitive(after.backward)) {
    forward.args <- base::formals(base::args(after.backward));
  } else {
    forward.args <- base::formals(after.backward);
  }
  if((base::length(forward.args) != 1L) ||
     (!(base::identical(base::names(forward.args), base::c("x"))))) {
    base::stop("after.backward function must have at exactly argument named 'x'.");
  }

  # check complexity
  if((!(base::is.integer(after.complexity))) || (after.complexity < 0L)) {
    base::stop("after.complexity must be positive integer or 0L.")
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
    result <- dataTransformeR::Transformation.new(forward = after.forward, backward = after.backward,
                                                  complexity = after.complexity);
  } else {
    result <- dataTransformeR::Transformation.new(
                forward = functionComposeR::function.compose(before@forward, after.forward),
                backward = functionComposeR::function.compose(after.backward, before@backward),
                complexity = (after.complexity + before@complexity));
  }

  result <- base::force(result);
  result@forward <- base::force(result@forward);
  result@backward <- base::force(result@backward);
  result@complexity <- base::force(result@complexity);
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
#' @importFrom methods validObject is
Transformation.andThen1 <- function(before, after) {
  # Some initial type checks
  if (base::missing(before) || base::is.null(before) ||
      (!(methods::is(before, "Transformation")))) {
    base::stop("'before' transformation is null or missing.");
  }
  if (base::missing(after) || base::is.null(after) ||
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
                                    after.forward = after@forward,
                                    after.backward = after@backward,
                                    after.complexity = after@complexity);
  result <- base::force(result);
  result@forward <- base::force(result@forward);
  result@backward <- base::force(result@backward);
  result@complexity <- base::force(result@complexity);
  return(result);
}
