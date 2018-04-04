library("dataTransformeR")
context("Transformation.applyDefault2D")


.do.test <- function(x, y) {
  x.min <- min(x);
  x.max <- max(x);
  x.norm <- (x - x.min)/(x.max-x.min);
  x.norm.neg <- 1 - x.norm;
  x.log <- (log(x)-log(x.min))/(log(x.max)-log(x.min));
  x.log.neg <- 1 - x.log;

  y.min <- min(y);
  y.max <- max(y);
  y.norm <- (y - y.min)/(y.max-y.min);
  y.norm.neg <- 1 - y.norm;
  y.log <- (log(y)-log(y.min))/(log(y.max)-log(y.min));
  y.log.neg <- 1 - y.log;

  result <- Transformation.applyDefault2D(x, y, FALSE);

  x.has_norm <- 0L;
  x.has_log <- 0L;
  x.has_norm_neg <- 0L;
  x.has_log_neg <- 0L;
  x.has_identity <- 0L;
  y.has_norm <- 0L;
  y.has_log <- 0L;
  y.has_norm_neg <- 0L;
  y.has_log_neg <- 0L;
  y.has_identity <- 0L;

  for(i in 1:length(result)) {
    test <- result[[i]];
    if(identical(test@x@data, x)) {
      x.has_identity <- x.has_identity + 1L;
      expect_identical(test@x@transformation@forward, identity);
      expect_identical(test@x@transformation@backward, identity);
    }
    if(isTRUE(all.equal(test@x@data, x.norm))) {
      x.has_norm <- x.has_norm + 1L;
    }
    if(isTRUE(all.equal(test@x@data, x.norm.neg))) {
      x.has_norm_neg <- x.has_norm_neg + 1L;
    }
    if(isTRUE(all.equal(test@x@data, x.log))) {
      x.has_log <- x.has_log + 1L;
    }
    if(isTRUE(all.equal(test@x@data, x.log.neg))) {
      x.has_log_neg <- x.has_log_neg + 1L;
    }

    if(identical(test@y@data, y)) {
      y.has_identity <- y.has_identity + 1L;
      expect_identical(test@y@transformation@forward, identity);
      expect_identical(test@y@transformation@backward, identity);
    }
    if(isTRUE(all.equal(test@y@data, y.norm))) {
      y.has_norm <- y.has_norm + 1L;
    }
    if(isTRUE(all.equal(test@y@data, y.norm.neg))) {
      y.has_norm_neg <- y.has_norm_neg + 1L;
    }
    if(isTRUE(all.equal(test@y@data, y.log))) {
      y.has_log <- y.has_log + 1L;
    }
    if(isTRUE(all.equal(test@y@data, y.log.neg))) {
      y.has_log_neg <- y.has_log_neg + 1L;
    }
  }
  expect_gt(x.has_norm, 0);
  expect_gte(x.has_norm_neg, 0);
  expect_gt(x.has_log, 0);
  expect_gte(x.has_log_neg, 0);
  expect_gt(y.has_norm, 0);
  expect_gte(y.has_norm_neg, 0);
  expect_gt(y.has_log, 0);
  expect_gte(y.has_log_neg, 0);

  result <- Transformation.applyDefault2D(x, y, TRUE);

  x.has_norm <- 0L;
  x.has_log <- 0L;
  x.has_norm_neg <- 0L;
  x.has_log_neg <- 0L;
  x.has_identity <- 0L;
  y.has_norm <- 0L;
  y.has_log <- 0L;
  y.has_norm_neg <- 0L;
  y.has_log_neg <- 0L;
  y.has_identity <- 0L;

  for(i in 1:length(result)) {
    test <- result[[i]];
    if(identical(test@x@data, x)) {
      x.has_identity <- x.has_identity + 1L;
      expect_identical(test@x@transformation@forward, identity);
      expect_identical(test@x@transformation@backward, identity);
    }
    if(isTRUE(all.equal(test@x@data, x.norm))) {
      x.has_norm <- x.has_norm + 1L;
    }
    if(isTRUE(all.equal(test@x@data, x.norm.neg))) {
      x.has_norm_neg <- x.has_norm_neg + 1L;
    }
    if(isTRUE(all.equal(test@x@data, x.log))) {
      x.has_log <- x.has_log + 1L;
    }
    if(isTRUE(all.equal(test@x@data, x.log.neg))) {
      x.has_log_neg <- x.has_log_neg + 1L;
    }

    if(identical(test@y@data, y)) {
      y.has_identity <- y.has_identity + 1L;
      expect_identical(test@y@transformation@forward, identity);
      expect_identical(test@y@transformation@backward, identity);
    }
    if(isTRUE(all.equal(test@y@data, y.norm))) {
      y.has_norm <- y.has_norm + 1L;
    }
    if(isTRUE(all.equal(test@y@data, y.norm.neg))) {
      y.has_norm_neg <- y.has_norm_neg + 1L;
    }
    if(isTRUE(all.equal(test@y@data, y.log))) {
      y.has_log <- y.has_log + 1L;
    }
    if(isTRUE(all.equal(test@y@data, y.log.neg))) {
      y.has_log_neg <- y.has_log_neg + 1L;
    }
  }

  expect_gt(x.has_identity, 0);
  expect_gt(x.has_norm, 0);
  expect_gte(x.has_norm_neg, 0);
  expect_gt(x.has_log, 0);
  expect_gte(x.has_log_neg, 0);
  expect_gt(y.has_identity, 0);
  expect_gt(y.has_norm, 0);
  expect_gte(y.has_norm_neg, 0);
  expect_gt(y.has_log, 0);
  expect_gte(y.has_log_neg, 0);
}

test_that("Test Transformation.applyDefault2D", {
  x <- c(1, 2, 3);
  y <- c(2, 3, 4);
  .do.test(x, y);
})

test_that("Test Transformation.applyDefault2D", {
  x <- c(0.1, 22, 13);
  y <- c(2, 5, 7);
  .do.test(x, y);
})
