#' @include Transformation.R
#' @include TransformedData.R
#' @include identity.R

#' @title Apply All the Provided Transformations
#' @description Provide all the provided transformation functions.
#' @param data the data
#' @param transformations the transformations
#' @param addIdentity should an identity transformation result be created as
#'   well? (by default \code{FALSE})
#' @return a list with the transformation results, or \code{NULL} if no
#'   transformation succeeded
#' @export Transformation.applyAll
Transformation.applyAll <- function(data, transformations, addIdentity=FALSE) {
  # Apply all the transformation functions to the data.
  result <- unlist(lapply(X=transformations, FUN=function(x) x(data)));
  # If the result is null or empty, we need to handle that
  if(is.null(result) || (length(result)<=0)) {
    # If we were asked to add the identity, we check if that can be done, i.e.,
    # if there are no non-finite data. In that case, we can simply only return
    # the raw data.
    if(addIdentity && is.finite(sum(range(data)))) {
      return(c(Transformation.identity(data=data)));
    }
    return(NULL);
  }

  # So there is at least one successful application of a transformation function.
  # We now check for duplicates, i.e., data sets with the same values.
  # We will remove those from the list.
  identityIndex <- -1L;
  for(i in length(result):1) {
    idata <- result[[i]]@data;
    k <- length(result);
    if(k > i) {
      for(j in k:(i+1)) {
        if(identical(result[[j]]@data, idata)) {
          result[[j]] <- NULL;
        }
      }
    }

    if(addIdentity && identical(data, idata)) {
      # If we should add the identity transformation AND we have found data that
      # would be equivalent to it ... then we should just replace that data,
      # because the identity transformation is going to be more efficient.
      identityIndex <- i;
    }
  }

  if(addIdentity) {
    # We shall add the identity transformation.
    if(identityIndex <= 0L) {
      # Add a new identity transformed data
      result[[length(result) + 1]] <- Transformation.identity(data=data);
    } else {
      # Replace the contents of the transformation result
      result[[identityIndex]]@data <- data;
      result[[identityIndex]]@transformation <- .Transformation.identity;
    }
  }

  result <- force(result);
  result <- unlist(result);
  result <- force(result);
  if(is.null(result) || (length(result) <= 0)) { return(NULL); }
  return(result);
}
