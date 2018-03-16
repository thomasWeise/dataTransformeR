#' @include Transformation.R

#' @title Create a Transformation which Shifts all Values of a Data Array into
#'   the Positive Zone
#' @description A transformation is created which could be used to shift all
#'   elements of a data vector into the positive range. This is a utility
#'   function which can be applied before log-scaling or \code{1/x} scaling of
#'   data. The transformation will be a linear shift and not involve scaling
#'   or squaring.
#' @param data the data vector
#' @param zeroAllowed is it ok if the transformation of the data vector would
#'   produce 0 values?
#' @return a \code{\link{Transformation}} which would transform the data vector
#'   into the positive domain, or \code{NULL} if no such transformation can be
#'   provided
#' @export Transformation.makePositive
#' @examples
#' data <- c(-1, 0, 0.1, 2)
#' trafo <- Transformation.makePositive(data, zeroAllowed=TRUE)
#' trafo@forward(data)
#' # [1] 0.0 1.0 1.1 3.0
#' trafo@backward(trafo@forward(data))
#' # [1] -1.0  0.0  0.1  2.0
#' trafo <- Transformation.makePositive(data, zeroAllowed=FALSE)
#' trafo@forward(data)
#' # [1] 0.0625 1.0625 1.1625 3.0625
#' trafo@backward(trafo@forward(data))
#' # [1] -1.0  0.0  0.1  2.0
Transformation.makePositive <- function(data, zeroAllowed=TRUE) {
  if(base::is.null(data) || (!(base::is.numeric(data) &&
                               base::is.vector(data) &&
                               (base::length(data) > 0L)))) {
    stop("data must be a vector of non-zero length.")
  }

  data.min <- +Inf;
  data.abs.min <- +Inf;

  # Find the minimum and the minimum absolute value
  for(value in data) {
    if(!base::is.finite(value)) {
      return(NULL);
    }
    if(value < data.min) {
      data.min <- value;
    }
    value.abs <- base::abs(value);
    if((value.abs > 0) && (value.abs < data.abs.min)) {
      data.abs.min <- value.abs;
    }
  }

  # Is the data broken?
  if(!(base::is.finite(data.min))) {
    # Broken data, all values are infinite values, let's return NULL.
    return(NULL);
  }

  # The data is all larger than 0, so we can apply the identity
  # transformation. We can also do this if the smallest value is 0
  # and zeros are allowed.
  if((data.min > 0) || (zeroAllowed && (data.min == 0))) {
    return(.Transformation.identity);
  }

  # All values are zero, but zero is not allowed
  if(!(base::is.finite(data.abs.min))) {
    # All values are zero, let's return a shift by 1
    return(Transformation.new(function(x) x+1, function(x) x-1));
  }

  # Let's flip the sign of the minimum, so it is positive.
  data.min <- base::abs(data.min);
  data.min <- base::force(data.min);

  # The data contains 0 or negative elements: min<=0.
  if(zeroAllowed) {
    # Zeros are allowed, so we can shift by -data.min.
    # This will turn the smallest value to 0 and all bigger values to
    # to positive.
    return(Transformation.new(function(x) x+data.min, function(x) x-data.min));
  }

  # We have negative values and are not allowed to produce zeros.
  # If we would (data - data.min), but then the result contains a 0 as well.
  # So we need to shift it by an offset.
  offset <- data.abs.min;
  offset <- base::force(offset);
  if(offset != 1) {
    # If the offset is not 1, we try to move it to the next smaller power
    # of two. This is not really necessary, but maybe makes for a more
    # elegant shifting of data and may lead to less loss of precision, as
    # powers of two should just have a single bit set in their binary
    # representation.
    offset.log.2 <- base::floor(base::log2(offset));
    if(base::is.finite(offset.log.2) &&
      (offset.log.2 > -32) && (offset.log.2 < 32)) {
      offset.test <- 2L ^ base::as.integer(offset.log.2);
      if(base::is.finite(offset.test) && (offset.test < offset)) {
        offset <- offset.test;
      }
    }
  }
  offset <- base::force(offset);

  if(data.min != 0) {
    # Test if we can combine offset and min without losing precision
    offset.test <- (offset + data.min);
    canCoerce <- FALSE;
    if((offset.test - data.min) > 0) {
      if(base::identical(offset.test - offset, data.min) &&
         base::identical(offset.test - data.min, offset)) {
        temp <- offset.test + offset;
        if(base::identical(temp - offset.test, offset) &&
           base::identical(temp - offset, offset.test)) {
          canCoerce = TRUE;
        }
      }
    }

    if(canCoerce) {
      offset.test <- base::force(offset.test);
      # We can use a single value, because we won't lose any precision.
      return(Transformation.new(forward = function(x) x + offset.test,
                                backward = function(x) x - offset.test));
    } else {
      # We need to first add data.min and then the offset, or we will lose
      # precision.
      return(Transformation.new(forward = function(x) (x + data.min) + offset,
                                backward = function(x) (x - offset) - data.min));
    }
  } #else
  # else: data.min == 0
  return(Transformation.new(forward = function(x) x + offset,
                            backward = function(x) x - offset));
}
