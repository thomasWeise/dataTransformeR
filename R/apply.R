#' @include Transformation.R
#' @include TransformedData.R
#' @include andThen.R
#' @include intervals.R

#' @title Apply a \code{\link{Transformation}} to a Data Vector and (Normalize the Result if Wanted)
#'
#' @description  We transform a vector \code{data} using a given
#' \code{\link{Transformation}}, i.e., create a \code{\link{TransformedData}}
#' object where all elements are the result of the \code{Transformation@forward}
#' function. If the parameter \code{normalize} is set to \code{TRUE}, which it
#' is by default, the transformed data will further be normalized and the
#' \code{transformation} is adapted accordingly. In this case, it is ensured
#' that all elements of the produced \code{TransformedData@data} vector will be
#' in \code{[0, 1]}. If the transformation fails, i.e., produces non-finite,
#' \code{NaN}, or \code{NA} values, \code{NULL} is returned.
#' @param data the data vector to normalize
#' @param transformation the transformation to be applied first
#' @param normalize normalize the result (and modify the transformation
#'   accordingly)? ... \code{TRUE} by default
#' @param negateNormalization if \code{normalize} is \code{TRUE}, this parameter
#'   decides whether the maximum transformed value should be mapped to \code{1}
#'   (\code{negateNormalization==FALSE}) or to \code{0}
#'   (\code{negateNormalization==TRUE})
#' @return the corresponding \code{\link{TransformedData}} instance
#' @export Transformation.apply
#' @importFrom methods is validObject
#' @examples
#' trafo <- Transformation.new(function(x) x*x, sqrt)
#' data <- c(1, 2, 3, 4)
#' Transformation.apply(data, trafo, normalize=FALSE)
#' # An object of class "TransformedData"
#' # Slot "transformation":
#' # An object of class "Transformation"
#' # Slot "forward":
#' # function (x)
#' # x * x
#' #
#' # Slot "backward":
#' # function (x)  .Primitive("sqrt")
#' #
#' # Slot "data":
#' # [1]  1  4  9 16
#' Transformation.apply(data, trafo, normalize=TRUE)
#' # An object of class "TransformedData"
#' # Slot "transformation":
#' # An object of class "Transformation"
#' # Slot "forward":
#' # function (x)
#' # ((x * x - 1)/15)
#' # <environment: 0x42a5ee8>
#' #
#' # Slot "backward":
#' # function (x)
#' # sqrt(x = (x * 15) + 1)
#' # <environment: 0x4085558>
#' #
#' #
#' # Slot "data":
#' # [1] 0.0000000 0.2000000 0.5333333 1.0000000
Transformation.apply <- function(data, transformation, normalize=TRUE, negateNormalization=FALSE) {
  # perform all the transformations of the data
  transformation <- base::force(transformation);
  if(base::is.null(transformation) ||
     (!methods::is(transformation, "Transformation"))) {
    stop("Transformation cannot be null and must be well-defined.")
  }
  methods::validObject(transformation);

  # check and setup the source data
  data <- base::force(data);
  if(base::is.null(data) || (!(base::is.numeric(data) &&
                               base::is.vector(data) &&
                               (base::length(data) > 0L)))) {
    stop("data must be a vector of non-zero length.")
  }

  # for each transformation function, we first create a transformed copy of the data
  if(base::identical(transformation@forward, identity)) {
    transformed.data <- base::force(data);
  } else {
    transformed.data <- base::vapply(X=data, FUN=transformation@forward, FUN.VALUE=NaN);
    transformed.data <- base::force(transformed.data);
  }

  # now we can compute the range of the data
  transformed.range <- base::range(transformed.data);
  transformed.min <- transformed.range[[1]];
  transformed.max <- transformed.range[[2]];

  if(!(base::is.finite(transformed.min) && base::is.finite(transformed.max))) {
    # data is not finite, this transformation cannot be used
    return(NULL);
  }

  if(normalize) {
    # if the range is empty, then we just return an array with all 0.5 values
    if(transformed.min >= transformed.max) {
      # allocate an array filled with 0.5
      transformed.data <- base::rep(0.5, base::length(data));
      data.range <- base::range(data);
      bwdv <- (0.5 * (data.range[[1]] + data.range[[2]]));
      transformation <- dataTransformeR::Transformation.new(
        forward=function(x) base::rep(0.5, base::length(x)),
        backward=function(x) base::rep(bwdv, base::length(x)));
    } else {
      # ok, if we get here, then the range of the transformed data was not empty
      if(negateNormalization) {
        temp <- transformed.min;
        transformed.min <- transformed.max;
        transformed.max <- temp;
      }
      normalization <- dataTransformeR::Transformation.normalizeInterval(transformed.min,
                                                                         transformed.max);

      # check if the normalization works in both directions, just to be sure
      normalized.min <- normalization@forward(transformed.min);
      if(!(base::is.finite(normalized.min) &&
           base::all.equal(normalized.min, 0) )) {
        return(NULL);
      }
      normalized.min.back <- normalization@backward(normalized.min);
      if(!(base::is.finite(normalized.min.back) &&
           base::all.equal(normalized.min.back, transformed.min))) {
        return(NULL);
      }

      normalized.max <- normalization@forward(transformed.max);
      if(!(base::is.finite(normalized.max) &&
           base::all.equal(normalized.max, 1) )) {
        return(NULL);
      }
      normalized.max.back <- normalization@backward(normalized.max);
      if(!(base::is.finite(normalized.max.back) &&
           base::all.equal(normalized.max.back, transformed.max))) {
        return(NULL);
      }

      # we now normalize the data into the range [0, 1]
      tryCatch({
        transformed.data <- base::vapply(X=transformed.data,
                                         FUN=function(x) {
                                           base::min(1, base::max(0, normalization@forward(x)))
                                         }, FUN.VALUE=NaN);
      }, error=function(e) { return(NULL); }, warning=function(e) { return(NULL); } );
      if(!(base::all(base::is.finite(transformed.data)))) {
        return(NULL);
      }
      transformation <- dataTransformeR::Transformation.andThen1(transformation, normalization);
    }
  }

  # we create the result and enforce that all expressions are REALLY evaluated
  result <- TransformedData.new(transformation=transformation,
                                data=transformed.data);
  result <- base::force(result);
  result@data <- base::force(result@data);
  result@transformation <- base::force(result@transformation);
  result@transformation@forward  <- base::force(result@transformation@forward);
  result@transformation@backward <- base::force(result@transformation@backward);

  return(result); # return the result
}
