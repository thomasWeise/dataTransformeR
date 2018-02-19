library("dataTransformeR")
context("Transformation.makePositive")


.do.test <- function(data) {
  data.min <- min(data);
  max.abs <- max(abs(data));

  result.allow.zero <- Transformation.makePositive(data, TRUE);
  result.no.zero <- Transformation.makePositive(data, FALSE);

  if(all(is.finite(data))) {
    expect_s4_class(result.allow.zero, "Transformation");
    validObject(result.allow.zero);
    for(d in data) {
      expect_gte(result.allow.zero@forward(d), 0);
    }
    transformed.allow.zero <- result.allow.zero@forward(data);
    expect_length(transformed.allow.zero, length(data));
    if(data.min >= 0) {
      expect_identical(result.allow.zero@forward, identity)
      expect_identical(result.allow.zero@backward, identity)
      expect_identical(transformed.allow.zero, data);
    }
    expect_identical(identical(result.allow.zero@forward, identity),
                     identical(result.allow.zero@backward, identity));


    expect_s4_class(result.no.zero, "Transformation");
    validObject(result.no.zero);
    for(d in data) {
      expect_gt(result.no.zero@forward(d), 0);
    }
    transformed.no.zero <- result.no.zero@forward(data);
    expect_length(transformed.no.zero, length(data));
    if(data.min > 0) {
      expect_identical(result.no.zero@forward, identity)
      expect_identical(result.no.zero@backward, identity)
      expect_identical(transformed.no.zero, data);
    }
    expect_identical(identical(result.no.zero@forward, identity),
                     identical(result.no.zero@backward, identity));
  } else {
    expect_null(result.allow.zero);
    expect_null(result.no.zero);
  }
}

test_that("Test Transformation.makePositive no transformation needed", {
  data <- c(1, 2, 3, 4);
  .do.test(data);
})

test_that("Test Transformation.makePositive c(0,...)", {
  data <- c(0, 1, 2, 3, 4);
  .do.test(data);
})

test_that("Test Transformation.makePositive c(-1,...)", {
  data <- c(-1, 0, 1, 2, 3, 4);
  .do.test(data);
})

test_that("Test Transformation.makePositive c(0,..., 1e20)", {
  data <- c(0, 1, 2, 3, 4, 1e20);
  .do.test(data);
})

test_that("Test Transformation.makePositive rnorm", {
  for(i in 1:20) {
    data <- rnorm(as.integer(runif(n=1, min=1, max=1000)));
    .do.test(data);
  }
})

test_that("Test Transformation.makePositive c(0, 1e-22,..., 1e20)", {
  data <- c(0, 1e-22, 1, 2, 3, 4, 1e20);
  .do.test(data);
})

test_that("Test Transformation.makePositive c(-1e20,..., 1e20)", {
  data <- c(-1e20, 0, 1, 2, 3, 4, 1e20);
  .do.test(data);
})

test_that("Test Transformation.makePositive c(-1e-22, ...)", {
  data <- c(-1e-22, 0, 1, 2, 3, 4);
  .do.test(data);
})

test_that("Test Transformation.makePositive c(-1e20,-1e-22,..., 1e20)", {
  data <- c(-1e20, -1e-22, 0, 1, 2, 3, 4, 1e20);
  .do.test(data);
})

test_that("Test Transformation.makePositive c(0)", {
  data <- c(0);
  .do.test(data);
})

test_that("Test Transformation.makePositive c(-1)", {
  data <- c(-1);
  .do.test(data);
})

test_that("Test Transformation.makePositive c(1)", {
  data <- c(1);
  .do.test(data);
})

test_that("Test Transformation.makePositive c(1, NaN)", {
  data <- c(1, NaN);
  .do.test(data);
})

test_that("Test Transformation.makePositive c(1, 2, +Inf, 3)", {
  data <- c(1, 2, +Inf, 3);
  .do.test(data);
})

test_that("Test Transformation.makePositive c(1, -Inf, 3)", {
  data <- c(1, -Inf, 3);
  .do.test(data);
})

test_that("Test Transformation.makePositive rnorm", {
  for(i in 1:20) {
    data <- runif(as.integer(runif(n=1, min=1, max=1000)));
    .do.test(data);
  }
})