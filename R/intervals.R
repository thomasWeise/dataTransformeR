#' @title Create a Bijective Mapping between two Intervals
#'
#' @description Create a bijective mapping between the two real intervals
#'   \code{[domainMin, domainMax]} and \code{[imageMin, imageMax]}. The returned
#'   mapping will map \code{domainMin} to \code{imageMin} and \code{domainMax}
#'   to \code{imageMax} and vice versa.
#'
#'   Notice that \code{domainMin>domainMax} as well as \code{imageMi >imageMax}
#'   are both permissible, in which case smaller input values may map to larger
#'   output values.
#'
#'   Furthermore, although we specify domain and image boundaries, the returned
#'   mapping will not have any boundaries. It is entirely allowed to map values
#'   smaller than \code{domainMin} or larger than \code{domainMax}. These may
#'   then land outside of \code{[imageMin, imageMax]}, though.
#'
#' @param domainMin the minimum of the original interval
#' @param domainMax the maximum of the original interval
#' @param imageMin the minimum of the goal interval
#' @param imageMax the maximum of the goal interval
#' @return a bijection between \code{[d, domainMax]} and \code{[0,1]}
#' @seealso \code{\link{Transformation.normalizeInterval}}
#' @export Transformation.mapIntervals
#' @examples
#' trafo <- Transformation.mapIntervals(-1, 1, 0, 2)
#' trafo
#' # An object of class "Transformation"
#' # Slot "forward":
#' # function (x)
#' # x + 1
#' # <environment: 0x35519c0>
#' #
#' # Slot "backward":
#' # function (x)
#' # x - 1
#' # <environment: 0x35519c0>
#' trafo <- Transformation.mapIntervals(0.2, 12, -0.1, 2)
#' trafo
#' # An object of class "Transformation"
#' # Slot "forward":
#' # function (x)
#' # (2.1 * ((x - 0.2)/11.8)) - 0.1
#' # <environment: 0x3c877f0>
#' #
#' # Slot "backward":
#' # function (x)
#' # 0.2 + (11.8 * ((x + 0.1)/2.1))
#' # <environment: 0x3c877f0>
Transformation.mapIntervals <- function(domainMin, domainMax, imageMin=0, imageMax=1) {
  if ((domainMin == imageMin) && (domainMax == imageMax)) {
    return(.Transformation.identity);
  }

  domainMin <- force(domainMin);
  domainMax <- force(domainMax);
  imageMin <- force(imageMin);
  imageMax <- force(imageMax);

  domainRange <- (domainMax - domainMin);
  domainRange <- force(domainRange);

  imageRange <- (imageMax - imageMin);
  imageRange <- force(imageRange);

  fwd <- NULL;
  bwd <- NULL;
  complexity <- 5L;

  if(domainRange == imageRange) {
    offset <- (imageMin - domainMin);
    if(offset == 0) {
      return(.Transformation.identity);
    }
    offset <- force(offset);
    if(offset > 0) {
      fwd <- function(x) x + offset;
      bwd <- function(x) x - offset;
      complexity <- 2L;
    } else {
      offset <- (-offset);
      fwd <- function(x) x - offset;
      bwd <- function(x) x + offset;
      complexity <- 2L;
    }
  } else {
    if(imageMin == 0) {
      if(domainMin == 0) {
        fwdConv <- imageRange / domainRange;
        if(fwdConv == 1) {
          return(.Transformation.identity);
        }
        fwdConv <- force(fwdConv);
        bwdConf <- domainRange / imageRange;
        bwdConv <- force(fwdConv);
        fwd <- function(x) x * fwdConv;
        bwd <- function(x) x * bwdConf;
        complexity <- 2L;
      } else {
        if(imageMax == 1) {
          if(domainMin > 0) {
            fwd <- function(x) ((x - domainMin) / domainRange);
            bwd <- function(x) (x * domainRange) + domainMin;
            complexity <- 3L;
          } else {
            domainMin <- (-domainMin);
            fwd <- function(x) ((x + domainMin) / domainRange);
            bwd <- function(x) (x * domainRange) - domainMin;
            complexity <- 3L;
          }
        }
      }
    }
    if(is.null(fwd)) {
      if( (domainMin == 0) && (domainMax == 1)) {
        if(imageMin > 0) {
          fwd <- function(x) imageMin + (imageRange * x);
          bwd <- function(x) (x - imageMin) / imageRange;
          complexity <- 3L;
        } else {
          imageMin <- (-imageMin);
          fwd <- function(x) (imageRange * x) - imageMin;
          bwd <- function(x) (x + imageMin) / imageRange;
          complexity <- 3L;
        }
      } else {
        if(domainRange == 1) {
          if(imageRange == 1) {
            if(imageMin > 0) {
              if(domainMin > 0) {
                fwd <- function(x) imageMin + (x - domainMin);
                bwd <- function(x) domainMin + (x - imageMin);
                complexity <- 3L;
              } else {
                domainMin <- (-domainMin);
                fwd <- function(x) imageMin + (x + domainMin);
                bwd <- function(x) (x - imageMin) - domainMin;
                complexity <- 3L;
              }
            } else {
              imageMin <- (-imageMin);
              if(domainMin > 0) {
                fwd <- function(x) (x - domainMin) - imageMin;
                bwd <- function(x) domainMin + (x + imageMin);
                complexity <- 3L;
              } else {
                domainMin <- (-domainMin);
                fwd <- function(x) (x + domainMin) - imageMin;
                bwd <- function(x) (x + imageMin) - domainMin;
                complexity <- 3L;
              }
            }
          } else {
            if(imageMin < 0) {
              imageMin <- (-imageMin);
              if(domainMin < 0) {
                domainMin <- (-domainMin);
                fwd <- function(x) (imageRange * (x + domainMin)) - imageMin;
                bwd <- function(x) ((x + imageMin) / imageRange) - domainMin;
                complexity <- 4L;
              } else {
                fwd <- function(x) (imageRange * (x - domainMin)) - imageMin;
                bwd <- function(x) domainMin + ((x + imageMin) / imageRange);
                complexity <- 4L;
              }
            } else {
              if(domainMin < 0){
                domainMin <- (-domainMin);
                fwd <- function(x) imageMin + (imageRange * (x + domainMin));
                bwd <- function(x) ((x - imageMin) / imageRange) - domainMin;
                complexity <- 4L;
              } else {
                fwd <- function(x) imageMin + (imageRange * (x - domainMin));
                bwd <- function(x) domainMin + ((x - imageMin) / imageRange);
                complexity <- 4L;
              }
            }
          }
        } else {
          if(imageRange == 1) {
            fwd <- function(x) imageMin + ((x - domainMin) / domainRange);
            bwd <- function(x) domainMin + (domainRange * (x - imageMin));
            complexity <- 4L;
          } else {
            if(imageMin < 0) {
              imageMin <- (-imageMin);
              if(domainMin < 0) {
                domainMin <- (-domainMin);
                fwd <- function(x) (imageRange * ((x + domainMin) / domainRange)) - imageMin;
                bwd <- function(x) (domainRange * ((x + imageMin) / imageRange)) - domainMin;
                complexity <- 5L;
              } else {
                fwd <- function(x) (imageRange * ((x - domainMin) / domainRange)) - imageMin;
                bwd <- function(x) domainMin + (domainRange * ((x + imageMin) / imageRange));
                complexity <- 5L;
              }
            } else {
              if(domainMin < 0){
                domainMin <- (-domainMin);
                fwd <- function(x) imageMin + (imageRange * ((x + domainMin) / domainRange));
                bwd <- function(x) (domainRange * ((x - imageMin) / imageRange)) - domainMin;
                complexity <- 5L;
              } else {
                fwd <- function(x) imageMin + (imageRange * ((x - domainMin) / domainRange));
                bwd <- function(x) domainMin + (domainRange * ((x - imageMin) / imageRange));
                complexity <- 5L;
              }
            }
          }
        }
      }
    }
  }

  result <- Transformation.new(forward = fwd, backward = bwd,
                                                complexity = complexity);
  result <- force(result);
  result@forward <- force(result@forward);
  result@backward <- force(result@backward);
  result@complexity <- force(result@complexity);

  return(result);
}

#' @title Create a Normalization Transformation
#'
#' @description Create a bijective mapping between the real interval
#'   \code{[domainMin, domainMax]} and \code{[0, 1]}. The returned mapping will
#'   map \code{domainMin} to \code{0} and \code{domainMax} to \code{1}. Notice
#'   that \code{domainMin>domainMax} is permissible, in which case smaller input
#'   values map to larger output values.
#'
#' @param domainMin the minimum of the original interval
#' @param domainMax the maximum of the original interval
#' @return a bijection between \code{[domainMin, domainMax]} and \code{[0,1]}
#' @seealso \code{\link{Transformation.mapIntervals}}
#' @export Transformation.normalizeInterval
#' @examples
#' Transformation.normalizeInterval(1, 3)
#' # An object of class "Transformation"
#' # Slot "forward":
#' # function (x)
#' # ((x - 1)/2)
#' # <environment: 0x3bf9960>
#' #
#' # Slot "backward":
#' # function (x)
#' # (x * 2) + 1
#' # <environment: 0x3bf9960>
Transformation.normalizeInterval <- function(domainMin, domainMax) {
  return(Transformation.mapIntervals(domainMin=domainMin, domainMax=domainMax,
                                     imageMin=0, imageMax=1))
}
