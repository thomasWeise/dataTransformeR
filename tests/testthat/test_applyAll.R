library("dataTransformeR")
context("Transformation.applyAll")

test_that("Test Transformation.applyAll with identity mapping", {
  data <- c(1, 2, 3);
  result.1 <- Transformation.applyAll(data, c(Transformation.normalize), FALSE);
  expect_length(result.1, 1);
  expect_s4_class(result.1[[1]], "TransformedData")
  expect_identical(result.1[[1]]@data, c(0, 0.5, 1));

  result.2 <- Transformation.applyAll(data, c(Transformation.normalize), TRUE);
  expect_length(result.2, 2);
  expect_s4_class(result.2[[1]], "TransformedData")
  expect_s4_class(result.2[[2]], "TransformedData")
  if(identical(result.2[[1]]@data, data)) {
    expect_identical(result.2[[1]]@transformation@forward, identity)
    expect_identical(result.2[[1]]@transformation@backward, identity)
    expect_identical(result.2[[2]]@data, result.1[[1]]@data);
    expect_equal(result.2[[2]]@transformation@forward, result.1[[1]]@transformation@forward);
    expect_equal(result.2[[2]]@transformation@backward, result.1[[1]]@transformation@backward);
  } else {
    expect_identical(result.2[[2]]@data, data);
    expect_identical(result.2[[2]]@transformation@forward, identity)
    expect_identical(result.2[[2]]@transformation@backward, identity)
    expect_identical(result.2[[1]]@data, result.1[[1]]@data);
    expect_equal(result.2[[1]]@transformation@forward, result.1[[1]]@transformation@forward);
    expect_equal(result.2[[1]]@transformation@backward, result.1[[1]]@transformation@backward);
  }
})


test_that("Test Transformation.applyAll with identity mapping", {
  data <- c(0, 0.5, 1);
  result.1 <- Transformation.applyAll(data, c(Transformation.normalize), FALSE);
  expect_length(result.1, 1);
  expect_s4_class(result.1[[1]], "TransformedData")
  expect_identical(result.1[[1]]@data, c(0, 0.5, 1));
  expect_identical(result.1[[1]]@transformation@forward, identity)
  expect_identical(result.1[[1]]@transformation@backward, identity)

  result.2 <- Transformation.applyAll(data, c(Transformation.normalize), TRUE);
  expect_length(result.2, 1);
  expect_s4_class(result.2[[1]], "TransformedData")
  expect_identical(result.2[[1]]@data, c(0, 0.5, 1));
  expect_identical(result.2[[1]]@transformation@forward, identity)
  expect_identical(result.2[[1]]@transformation@backward, identity)
})



test_that("Test Transformation.applyAll with identity mapping and +Inf", {
  data <- c(0, +Inf, 1);
  result.1 <- Transformation.applyAll(data, c(Transformation.normalize), FALSE);
  expect_null(result.1);

  result.2 <- Transformation.applyAll(data, c(Transformation.normalize), TRUE);
  expect_null(result.2);
})

test_that("Test Transformation.applyAll with identity mapping and -Inf", {
  data <- c(0, -Inf, 1);
  result.1 <- Transformation.applyAll(data, c(Transformation.normalize), FALSE);
  expect_null(result.1);

  result.2 <- Transformation.applyAll(data, c(Transformation.normalize), TRUE);
  expect_null(result.2);
})

test_that("Test Transformation.applyAll with identity mapping and NaN", {
  data <- c(0, NaN, 1);
  result.1 <- Transformation.applyAll(data, c(Transformation.normalize), FALSE);
  expect_null(result.1);

  result.2 <- Transformation.applyAll(data, c(Transformation.normalize), TRUE);
  expect_null(result.2);
})

test_that("Test Transformation.applyAll with identity mapping and NA", {
  data <- c(0, NA, 1);
  result.1 <- Transformation.applyAll(data, c(Transformation.normalize), FALSE);
  expect_null(result.1);

  result.2 <- Transformation.applyAll(data, c(Transformation.normalize), TRUE);
  expect_null(result.2);
})




test_that("Test Transformation.applyAll with identity mapping and negated identity mapping", {
  data <- c(1, 2, 3);
  result.1 <- Transformation.applyAll(data, c(Transformation.normalize, Transformation.normalizeNegated), FALSE);
  expect_length(result.1, 2);
  expect_s4_class(result.1[[1]], "TransformedData")
  expect_s4_class(result.1[[2]], "TransformedData")
  if(identical(result.1[[1]]@data, c(0, 0.5, 1))) {
    expect_identical(result.1[[1]]@data, c(0, 0.5, 1));
    expect_identical(result.1[[2]]@data, c(1, 0.5, 0));
  } else {
    expect_identical(result.1[[1]]@data, c(1, 0.5, 0));
    expect_identical(result.1[[2]]@data, c(0, 0.5, 1));
  }

  result.2 <- Transformation.applyAll(data, c(Transformation.normalize, Transformation.normalizeNegated), TRUE);
  expect_length(result.2, 3);
  expect_s4_class(result.2[[1]], "TransformedData")
  expect_s4_class(result.2[[2]], "TransformedData")
  expect_s4_class(result.2[[3]], "TransformedData")
  has_norm <- FALSE;
  has_norm_neg <- FALSE;
  has_identity <- FALSE;
  for(i in 1:3) {
    test <- result.2[[i]];
    if(identical(test@data, data)) {
      expect_identical(test@transformation@forward, identity);
      expect_identical(test@transformation@backward, identity);
      has_identity <- TRUE;
    } else {
      if(identical(test@data, c(1, 0.5, 0))) {
        has_norm_neg <- TRUE;
      } else {
        if(identical(test@data, c(0, 0.5, 1))) {
          has_norm <- TRUE;
        } else {
          stop("error: unknown result")
        }
      }
    }
  }
  expect_true(has_norm && has_norm_neg && has_identity);
})



test_that("Test Transformation.applyAll with normalized data, identity mapping and negated identity mapping", {
  data <- c(0, 0.2, 0.3, 1);
  result.1 <- Transformation.applyAll(data, c(Transformation.normalize, Transformation.normalizeNegated), FALSE);
  expect_length(result.1, 2);
  expect_s4_class(result.1[[1]], "TransformedData")
  expect_s4_class(result.1[[2]], "TransformedData")
  if(identical(result.1[[1]]@data, c(0, 0.2, 0.3, 1))) {
    expect_identical(result.1[[1]]@data, c(0, 0.2, 0.3, 1));
    expect_identical(result.1[[1]]@transformation@forward, identity)
    expect_identical(result.1[[1]]@transformation@backward, identity)
    expect_identical(result.1[[2]]@data, c(1, 0.8, 0.7, 0));
  } else {
    expect_identical(result.1[[1]]@data, c(1, 0.8, 0.7, 0));
    expect_identical(result.1[[2]]@data, c(0, 0.2, 0.3, 1));
    expect_identical(result.1[[2]]@transformation@forward, identity)
    expect_identical(result.1[[2]]@transformation@backward, identity)
  }

  result.2 <- Transformation.applyAll(data, c(Transformation.normalize, Transformation.normalizeNegated), TRUE);
  expect_length(result.2, 2);
  expect_s4_class(result.2[[1]], "TransformedData")
  expect_s4_class(result.2[[2]], "TransformedData")
  if(identical(result.2[[1]]@data, c(0, 0.2, 0.3, 1))) {
    expect_identical(result.2[[1]]@data, c(0, 0.2, 0.3, 1));
    expect_identical(result.2[[1]]@transformation@forward, identity)
    expect_identical(result.2[[1]]@transformation@backward, identity)
    expect_identical(result.2[[2]]@data, c(1, 0.8, 0.7, 0));
  } else {
    expect_identical(result.2[[1]]@data, c(1, 0.8, 0.7, 0));
    expect_identical(result.2[[2]]@data, c(0, 0.2, 0.3, 1));
    expect_identical(result.2[[2]]@transformation@forward, identity)
    expect_identical(result.2[[2]]@transformation@backward, identity)
  }
})


test_that("Test Transformation.applyAll with constant data", {
  data <- c(1, 1, 1);
  result.1 <- Transformation.applyAll(data, c(Transformation.normalize, Transformation.normalizeNegated), FALSE);
  expect_length(result.1, 1);
  expect_s4_class(result.1[[1]], "TransformedData")
  expect_identical(result.1[[1]]@data, c(0.5, 0.5, 0.5));

  result.2 <- Transformation.applyAll(data, c(Transformation.normalize, Transformation.normalizeNegated), TRUE);
  expect_length(result.2, 2);
  expect_s4_class(result.2[[1]], "TransformedData")
  expect_s4_class(result.2[[2]], "TransformedData")
  if(identical(result.2[[1]]@data, data)) {
    expect_identical(result.2[[1]]@data, data);
    expect_identical(result.2[[1]]@transformation@forward, identity)
    expect_identical(result.2[[1]]@transformation@backward, identity)
    expect_identical(result.2[[2]]@data, c(0.5, 0.5, 0.5));
  } else {
    expect_identical(result.2[[1]]@data, c(0.5, 0.5, 0.5));
    expect_identical(result.2[[2]]@data, data);
    expect_identical(result.2[[2]]@transformation@forward, identity)
    expect_identical(result.2[[2]]@transformation@backward, identity)
  }
})