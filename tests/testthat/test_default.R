library("dataTransformeR")
context("Transformation.applyDefault")


.do.test <- function(data) {
  data.min <- min(data);
  data.max <- max(data);
  data.norm <- (data - data.min)/(data.max-data.min);
  data.norm.neg <- 1 - data.norm;
  data.log <- (log(data)-log(data.min))/(log(data.max)-log(data.min));
  data.log.neg <- 1 - data.log;

  result <- Transformation.applyDefault(data, FALSE);

  has_norm <- FALSE;
  has_log <- FALSE;
  has_norm_neg <- FALSE;
  has_log_neg <- FALSE;
  has_identity <- FALSE;
  for(i in 1:length(result)) {
    test <- result[[i]];
    if(identical(test@data, data)) {
      has_identity <- TRUE;
      expect_identical(test@transformation@forward, identity);
      expect_identical(test@transformation@backward, identity);
    }
    if(isTRUE(all.equal(test@data, data.norm))) {
      has_norm <- TRUE;
    }
    if(isTRUE(all.equal(test@data, data.norm.neg))) {
      has_norm_neg <- TRUE;
    }
    if(isTRUE(all.equal(test@data, data.log))) {
      has_log <- TRUE;
    }
    if(isTRUE(all.equal(test@data, data.log.neg))) {
      has_log_neg <- TRUE;
    }
  }
  expect_true(has_norm && has_log);# && has_norm_neg && has_log_neg);

  result <- Transformation.applyDefault(data, TRUE);

  has_norm <- FALSE;
  has_log <- FALSE;
  has_norm_neg <- FALSE;
  has_log_neg <- FALSE;
  has_identity <- FALSE;
  for(i in 1:length(result)) {
    test <- result[[i]];
    if(identical(test@data, data)) {
      has_identity <- TRUE;
      expect_identical(test@transformation@forward, identity);
      expect_identical(test@transformation@backward, identity);
    }
    if(isTRUE(all.equal(test@data, data.norm))) {
      has_norm <- TRUE;
    }
    if(isTRUE(all.equal(test@data, data.norm.neg))) {
      has_norm_neg <- TRUE;
    }
    if(isTRUE(all.equal(test@data, data.log))) {
      has_log <- TRUE;
    }
    if(isTRUE(all.equal(test@data, data.log.neg))) {
      has_log_neg <- TRUE;
    }
  }
  expect_true(has_norm && has_log);# && has_norm_neg && has_log_neg && has_identity);
}

test_that("Test Transformation.applyDefault", {
  data <- c(1, 2, 3);
  .do.test(data);
})

test_that("Test Transformation.applyDefault", {
  data <- c(0.1, 22, 13);
  .do.test(data);
})

test_that("Test Transformation.applyDefault", {
  data <- c(0, 0.5, 1);
  data.min <- min(data);
  data.max <- max(data);
  data.norm <- (data - data.min)/(data.max-data.min);
  data.norm.neg <- 1 - data.norm;

  result <- Transformation.applyDefault(data, FALSE);

  has_norm <- FALSE;
  has_log <- FALSE;
  has_identity <- FALSE;
  for(i in 1:length(result)) {
    test <- result[[i]];
    if(identical(test@data, data)) {
      has_identity <- TRUE;
      expect_identical(test@transformation@forward, identity);
      expect_identical(test@transformation@backward, identity);
    }
    if(isTRUE(all.equal(test@data, data.norm))) {
      has_norm <- TRUE;
    }
    if(isTRUE(all.equal(test@data, data.norm.neg))) {
      has_norm_neg <- TRUE;
    }
  }
  expect_true(has_norm);# && has_norm_neg);

  result <- Transformation.applyDefault(data, TRUE);

  has_norm <- FALSE;
  has_log <- FALSE;
  has_identity <- FALSE;
  for(i in 1:length(result)) {
    test <- result[[i]];
    if(identical(test@data, data)) {
      has_identity <- TRUE;
      expect_identical(test@transformation@forward, identity);
      expect_identical(test@transformation@backward, identity);
    }
    if(isTRUE(all.equal(test@data, data.norm))) {
      has_norm <- TRUE;
    }
    if(isTRUE(all.equal(test@data, data.norm.neg))) {
      has_norm_neg <- TRUE;
    }
  }
  expect_true(has_norm && has_identity); # && has_norm_neg
})
