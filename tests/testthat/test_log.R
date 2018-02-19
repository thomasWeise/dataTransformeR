library("dataTransformeR")
context("Transformation.log")

test_that("Test Transformation.log (I)", {
  data <- c(1, 2, 3, 4);
  transformation <- Transformation.new(function(x) log(x), exp);

  expected <- Transformation.apply(data, transformation, TRUE, FALSE);
  expect_s4_class(expected, "TransformedData")
  validObject(expected);
  result <- Transformation.log(data);
  expect_s4_class(result, "TransformedData")
  validObject(result);

  expect_identical(expected@data, result@data);
  expect_identical(result@transformation@forward(data), expected@data);
  expect_equal(result@transformation@backward(result@data), data);
  log.min <- log(min(data));
  log.max <- log(max(data));
  log.range <- log.max - log.min;
  expect_identical(result@data, c( (log(1)-log.min)/log.range,
                                   (log(2)-log.min)/log.range,
                                   (log(3)-log.min)/log.range,
                                   (log(4)-log.min)/log.range));

  expected <- Transformation.apply(data, transformation, TRUE, TRUE);
  expect_s4_class(expected, "TransformedData")
  validObject(expected);
  result <- Transformation.logNegated(data);
  expect_s4_class(result, "TransformedData")
  validObject(result);
  expect_identical(expected@data, result@data);
  expect_identical(result@transformation@forward(data), expected@data);
  expect_equal(result@transformation@backward(result@data), data);
  expect_equal(result@data, c( 1-(log(1)-log.min)/log.range,
                                   1-(log(2)-log.min)/log.range,
                                   1-(log(3)-log.min)/log.range,
                                   1-(log(4)-log.min)/log.range));
})


test_that("Test Transformation.log (II)", {
  data <- c(0, 1, 2, 3, 4);
  normalized <- data + 1

  result <- Transformation.log(data);
  expect_s4_class(result, "TransformedData")
  validObject(result);

  expect_identical(result@transformation@forward(data), result@data);
  expect_equal(result@transformation@backward(result@data), data);
  log.min <- log(min(normalized));
  log.max <- log(max(normalized));
  log.range <- log.max - log.min;
  expect_identical(result@data, c( (log(normalized[1])-log.min)/log.range,
                                   (log(normalized[2])-log.min)/log.range,
                                   (log(normalized[3])-log.min)/log.range,
                                   (log(normalized[4])-log.min)/log.range,
                                   (log(normalized[5])-log.min)/log.range));

  result <- Transformation.logNegated(data);
  expect_s4_class(result, "TransformedData")
  validObject(result);
  expect_identical(result@transformation@forward(data), result@data);
  expect_equal(result@transformation@backward(result@data), data);
  expect_equal(result@data, c( 1-(log(normalized[1])-log.min)/log.range,
                                   1-(log(normalized[2])-log.min)/log.range,
                                   1-(log(normalized[3])-log.min)/log.range,
                                   1-(log(normalized[4])-log.min)/log.range,
                                   1-(log(normalized[5])-log.min)/log.range));
})




test_that("Test Transformation.log (III)", {
  data <- c(-1e-17, 0, 1, 2, 3);
  normalized <- data + 2e-17

  result <- Transformation.log(data);
  expect_s4_class(result, "TransformedData")
  validObject(result);

  expect_identical(result@transformation@forward(data), result@data);
  expect_equal(result@transformation@backward(result@data), data);
  log.min <- log(min(normalized));
  log.max <- log(max(normalized));
  log.range <- log.max - log.min;
  expect_identical(result@data, c( (log(normalized[1])-log.min)/log.range,
                                   (log(normalized[2])-log.min)/log.range,
                                   (log(normalized[3])-log.min)/log.range,
                                   (log(normalized[4])-log.min)/log.range,
                                   (log(normalized[5])-log.min)/log.range));

  result <- Transformation.logNegated(data);
  expect_s4_class(result, "TransformedData")
  validObject(result);
  expect_identical(result@transformation@forward(data), result@data);
  expect_equal(result@transformation@backward(result@data), data);
  expect_equal(result@data, c( 1-(log(normalized[1])-log.min)/log.range,
                               1-(log(normalized[2])-log.min)/log.range,
                               1-(log(normalized[3])-log.min)/log.range,
                               1-(log(normalized[4])-log.min)/log.range,
                               1-(log(normalized[5])-log.min)/log.range));
})