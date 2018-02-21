library("dataTransformeR")
context("Transformation.intervals")


test_that("Test Transformation.mapIntervals (I)", {
  vec.base <- -5:5

  vec.1 <- rep(vec.base, times=length(vec.base));
  vec.2 <- rep(vec.base, times=rep(length(vec.base), length(vec.base)));

  vec.1.1 <- rep(vec.1, times=length(vec.1));
  vec.1.2 <- rep(vec.1, times=rep(length(vec.1), length(vec.1)));
  vec.2.1 <- rep(vec.2, times=length(vec.2));
  vec.2.2 <- rep(vec.2, times=rep(length(vec.2), length(vec.2)));

  indices <- 1:length(vec.1.1);
  trafos <- lapply(X=indices, FUN=function(i) {
    if(vec.1.1[i]==vec.1.2[i]) return(NULL);
    if(vec.2.1[i]==vec.2.2[i]) return(NULL);
    return(Transformation.mapIntervals(vec.1.1[i], vec.1.2[i],
                                       vec.2.1[i], vec.2.2[i]));
  });

  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.1[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.1[i]));
                      },
                      FUN.VALUE = NaN), vec.2.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.2[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.2[i]));
                      },
                      FUN.VALUE = NaN), vec.2.2);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.1[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.1[i]));
                      },
                      FUN.VALUE = NaN), vec.1.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.2[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.2[i]));
                      },
                      FUN.VALUE = NaN), vec.1.2);

  vec.1.1 <- vec.1.1 * 0.2;
  vec.1.2 <- vec.1.2 * 0.2;
  vec.2.1 <- vec.2.1 * 0.2;
  vec.2.2 <- vec.2.2 * 0.2;
  trafos <- lapply(X=indices, FUN=function(i) {
    if(vec.1.1[i]==vec.1.2[i]) return(NULL);
    if(vec.2.1[i]==vec.2.2[i]) return(NULL);
    return(Transformation.mapIntervals(vec.1.1[i], vec.1.2[i],
                                       vec.2.1[i], vec.2.2[i]));
  });

  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.1[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.1[i]));
                      },
                      FUN.VALUE = NaN), vec.2.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.2[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.2[i]));
                      },
                      FUN.VALUE = NaN), vec.2.2);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.1[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.1[i]));
                      },
                      FUN.VALUE = NaN), vec.1.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.2[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.2[i]));
                      },
                      FUN.VALUE = NaN), vec.1.2);
})


test_that("Test Transformation.mapIntervals (II)", {
  vec.1.1 <- rnorm(100);
  vec.1.2 <- rnorm(100);
  vec.2.1 <- rnorm(100);
  vec.2.2 <- rnorm(100);
  indices <- 1:length(vec.1.1);
  trafos <- lapply(X=indices, FUN=function(i) {
    if(vec.1.1[i]==vec.1.2[i]) return(NULL);
    if(vec.2.1[i]==vec.2.2[i]) return(NULL);
    return(Transformation.mapIntervals(vec.1.1[i], vec.1.2[i],
                                       vec.2.1[i], vec.2.2[i]));
  });

  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.1[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.1[i]));
                      },
                      FUN.VALUE = NaN), vec.2.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.2[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.2[i]));
                      },
                      FUN.VALUE = NaN), vec.2.2);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.1[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.1[i]));
                      },
                      FUN.VALUE = NaN), vec.1.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.2[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.2[i]));
                      },
                      FUN.VALUE = NaN), vec.1.2);
})


test_that("Test Transformation.mapIntervals (III)", {
  vec.1.1 <- runif(100);
  vec.1.2 <- runif(100);
  vec.2.1 <- runif(100);
  vec.2.2 <- runif(100);
  indices <- 1:length(vec.1.1);
  trafos <- lapply(X=indices, FUN=function(i) {
    if(vec.1.1[i]==vec.1.2[i]) return(NULL);
    if(vec.2.1[i]==vec.2.2[i]) return(NULL);
    return(Transformation.mapIntervals(vec.1.1[i], vec.1.2[i],
                                       vec.2.1[i], vec.2.2[i]));
  });

  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.1[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.1[i]));
                      },
                      FUN.VALUE = NaN), vec.2.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.2[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.2[i]));
                      },
                      FUN.VALUE = NaN), vec.2.2);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.1[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.1[i]));
                      },
                      FUN.VALUE = NaN), vec.1.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.2[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.2[i]));
                      },
                      FUN.VALUE = NaN), vec.1.2);
})

#
# test_that("Test Transformation.mapIntervals (IV)", {
#   vec.base <- c(-1e20, -1, 0, 1, 1e40);
#
# vec.1 <- rep(vec.base, times=length(vec.base));
# vec.2 <- rep(vec.base, times=rep(length(vec.base), length(vec.base)));
#
# vec.1.1 <- rep(vec.1, times=length(vec.1));
# vec.1.2 <- rep(vec.1, times=rep(length(vec.1), length(vec.1)));
# vec.2.1 <- rep(vec.2, times=length(vec.2));
# vec.2.2 <- rep(vec.2, times=rep(length(vec.2), length(vec.2)));
#
# indices <- 1:length(vec.1.1);
# trafos <- lapply(X=indices, FUN=function(i) {
#   if(vec.1.1[i]==vec.1.2[i]) return(NULL);
#   if(vec.2.1[i]==vec.2.2[i]) return(NULL);
#   return(Transformation.mapIntervals(vec.1.1[i], vec.1.2[i],
#                                      vec.2.1[i], vec.2.2[i]));
# });
#
# expect_equal(vapply(X=indices,
#                     FUN=function(i) {
#                       if(is.null(trafos[[i]])) {
#                         return(vec.2.1[i]);
#                       }
#                       return(trafos[[i]]@forward(vec.1.1[i]));
#                     },
#                     FUN.VALUE = NaN), vec.2.1);
# expect_equal(vapply(X=indices,
#                     FUN=function(i) {
#                       if(is.null(trafos[[i]])) {
#                         return(vec.2.2[i]);
#                       }
#                       return(trafos[[i]]@forward(vec.1.2[i]));
#                     },
#                     FUN.VALUE = NaN), vec.2.2);
# expect_equal(vapply(X=indices,
#                     FUN=function(i) {
#                       if(is.null(trafos[[i]])) {
#                         return(vec.1.1[i]);
#                       }
#                       return(trafos[[i]]@backward(vec.2.1[i]));
#                     },
#                     FUN.VALUE = NaN), vec.1.1);
# expect_equal(vapply(X=indices,
#                     FUN=function(i) {
#                       if(is.null(trafos[[i]])) {
#                         return(vec.1.2[i]);
#                       }
#                       return(trafos[[i]]@backward(vec.2.2[i]));
#                     },
#                     FUN.VALUE = NaN), vec.1.2);
#
# vec.1.1 <- vec.1.1 * 0.2;
# vec.1.2 <- vec.1.2 * 0.2;
# vec.2.1 <- vec.2.1 * 0.2;
# vec.2.2 <- vec.2.2 * 0.2;
# trafos <- lapply(X=indices, FUN=function(i) {
#   if(vec.1.1[i]==vec.1.2[i]) return(NULL);
#   if(vec.2.1[i]==vec.2.2[i]) return(NULL);
#   return(Transformation.mapIntervals(vec.1.1[i], vec.1.2[i],
#                                      vec.2.1[i], vec.2.2[i]));
# });
#
# expect_equal(vapply(X=indices,
#                     FUN=function(i) {
#                       if(is.null(trafos[[i]])) {
#                         return(vec.2.1[i]);
#                       }
#                       return(trafos[[i]]@forward(vec.1.1[i]));
#                     },
#                     FUN.VALUE = NaN), vec.2.1);
# expect_equal(vapply(X=indices,
#                     FUN=function(i) {
#                       if(is.null(trafos[[i]])) {
#                         return(vec.2.2[i]);
#                       }
#                       return(trafos[[i]]@forward(vec.1.2[i]));
#                     },
#                     FUN.VALUE = NaN), vec.2.2);
# expect_equal(vapply(X=indices,
#                     FUN=function(i) {
#                       if(is.null(trafos[[i]])) {
#                         return(vec.1.1[i]);
#                       }
#                       return(trafos[[i]]@backward(vec.2.1[i]));
#                     },
#                       FUN.VALUE = NaN), vec.1.1);
# expect_equal(vapply(X=indices,
#                     FUN=function(i) {
#                       if(is.null(trafos[[i]])) {
#                         return(vec.1.2[i]);
#                       }
#                       return(trafos[[i]]@backward(vec.2.2[i]));
#                     },
#                     FUN.VALUE = NaN), vec.1.2);
# })



test_that("Test Transformation.mapIntervals (V)", {
  vec.base <- c(-1.1, -1e-23, 0, 1e-32, 1.05);

  vec.1 <- rep(vec.base, times=length(vec.base));
  vec.2 <- rep(vec.base, times=rep(length(vec.base), length(vec.base)));

  vec.1.1 <- rep(vec.1, times=length(vec.1));
  vec.1.2 <- rep(vec.1, times=rep(length(vec.1), length(vec.1)));
  vec.2.1 <- rep(vec.2, times=length(vec.2));
  vec.2.2 <- rep(vec.2, times=rep(length(vec.2), length(vec.2)));

  indices <- 1:length(vec.1.1);
  trafos <- lapply(X=indices, FUN=function(i) {
    if(vec.1.1[i]==vec.1.2[i]) return(NULL);
    if(vec.2.1[i]==vec.2.2[i]) return(NULL);
    return(Transformation.mapIntervals(vec.1.1[i], vec.1.2[i],
                                       vec.2.1[i], vec.2.2[i]));
  });

  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.1[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.1[i]));
                      },
                      FUN.VALUE = NaN), vec.2.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.2[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.2[i]));
                      },
                      FUN.VALUE = NaN), vec.2.2);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.1[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.1[i]));
                      },
                      FUN.VALUE = NaN), vec.1.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.2[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.2[i]));
                      },
                      FUN.VALUE = NaN), vec.1.2);

  vec.1.1 <- vec.1.1 * 0.2;
  vec.1.2 <- vec.1.2 * 0.2;
  vec.2.1 <- vec.2.1 * 0.2;
  vec.2.2 <- vec.2.2 * 0.2;
  trafos <- lapply(X=indices, FUN=function(i) {
    if(vec.1.1[i]==vec.1.2[i]) return(NULL);
    if(vec.2.1[i]==vec.2.2[i]) return(NULL);
    return(Transformation.mapIntervals(vec.1.1[i], vec.1.2[i],
                                       vec.2.1[i], vec.2.2[i]));
  });

  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.1[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.1[i]));
                      },
                      FUN.VALUE = NaN), vec.2.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.2[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.2[i]));
                      },
                      FUN.VALUE = NaN), vec.2.2);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.1[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.1[i]));
                      },
                      FUN.VALUE = NaN), vec.1.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.2[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.2[i]));
                      },
                      FUN.VALUE = NaN), vec.1.2);
})


test_that("Test Transformation.normalizeInterval (I)", {
  vec.base <- -5:5

  vec.1 <- rep(vec.base, times=length(vec.base));

  vec.1.1 <- rep(vec.1, times=length(vec.1));
  vec.1.2 <- rep(vec.1, times=rep(length(vec.1), length(vec.1)));
  vec.2.1 <- rep(0, times=length(vec.1.1));
  vec.2.2 <- rep(1, times=length(vec.1.2));

  indices <- 1:length(vec.1.1);
  trafos <- lapply(X=indices, FUN=function(i) {
    if(vec.1.1[i]==vec.1.2[i]) return(NULL);
    return(Transformation.normalizeInterval(vec.1.1[i], vec.1.2[i]));
  });

  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.1[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.1[i]));
                      },
                      FUN.VALUE = NaN), vec.2.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.2[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.2[i]));
                      },
                      FUN.VALUE = NaN), vec.2.2);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.1[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.1[i]));
                      },
                      FUN.VALUE = NaN), vec.1.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.2[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.2[i]));
                      },
                      FUN.VALUE = NaN), vec.1.2);
})




test_that("Test Transformation.normalizeInterval (II)", {
  vec.1.1 <- rnorm(100);
  vec.1.2 <- rnorm(100);
  vec.2.1 <- rep(0, 100);
  vec.2.2 <- rep(1, 100);
  indices <- 1:length(vec.1.1);
  trafos <- lapply(X=indices, FUN=function(i) {
    if(vec.1.1[i]==vec.1.2[i]) return(NULL);
    if(vec.2.1[i]==vec.2.2[i]) return(NULL);
    return(Transformation.mapIntervals(vec.1.1[i], vec.1.2[i],
                                       vec.2.1[i], vec.2.2[i]));
  });

  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.1[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.1[i]));
                      },
                      FUN.VALUE = NaN), vec.2.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.2[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.2[i]));
                      },
                      FUN.VALUE = NaN), vec.2.2);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.1[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.1[i]));
                      },
                      FUN.VALUE = NaN), vec.1.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.2[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.2[i]));
                      },
                      FUN.VALUE = NaN), vec.1.2);
})


test_that("Test Transformation.normalizeInterval (III)", {
  vec.1.1 <- runif(100);
  vec.1.2 <- runif(100);
  vec.2.1 <- rep(0, 100);
  vec.2.2 <- rep(1, 100);
  indices <- 1:length(vec.1.1);
  trafos <- lapply(X=indices, FUN=function(i) {
    if(vec.1.1[i]==vec.1.2[i]) return(NULL);
    if(vec.2.1[i]==vec.2.2[i]) return(NULL);
    return(Transformation.mapIntervals(vec.1.1[i], vec.1.2[i],
                                       vec.2.1[i], vec.2.2[i]));
  });

  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.1[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.1[i]));
                      },
                      FUN.VALUE = NaN), vec.2.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.2.2[i]);
                        }
                        return(trafos[[i]]@forward(vec.1.2[i]));
                      },
                      FUN.VALUE = NaN), vec.2.2);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.1[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.1[i]));
                      },
                      FUN.VALUE = NaN), vec.1.1);
  expect_equal(vapply(X=indices,
                      FUN=function(i) {
                        if(is.null(trafos[[i]])) {
                          return(vec.1.2[i]);
                        }
                        return(trafos[[i]]@backward(vec.2.2[i]));
                      },
                      FUN.VALUE = NaN), vec.1.2);
})
