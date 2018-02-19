library("dataTransformeR")
context("Transformation.normalize")

test_that("Test Transformation.normalize (I)", {
  for(i in 1:10) {
    data <- rnorm(100);
    data.range <- range(data);
    transformation <- Transformation.new(identity, identity);

    expected <- Transformation.apply(data, transformation, TRUE, FALSE);
    expect_s4_class(expected, "TransformedData")
    validObject(expected);

    result <- Transformation.normalize(data);
    expect_s4_class(result, "TransformedData")
    validObject(result);

    expect_identical(expected@data, result@data);
    expect_equal(result@transformation@forward(data), expected@data);
    expect_equal(result@transformation@backward(result@data), data);

    expected <- Transformation.apply(data, transformation, TRUE, TRUE);
    expect_s4_class(expected, "TransformedData")
    validObject(expected);

    result <- Transformation.normalizeNegated(data);
    expect_s4_class(result, "TransformedData")
    validObject(result);

    expect_identical(expected@data, result@data);
    expect_equal(result@transformation@forward(data), expected@data);
    expect_equal(result@transformation@backward(result@data), data);
  }
})

test_that("Test Transformation.normalize (II)", {
  for(i in 1:10) {
    data <- runif(100);
    data.range <- range(data);
    transformation <- Transformation.new(identity, identity);

    expected <- Transformation.apply(data, transformation, TRUE, FALSE);
    expect_s4_class(expected, "TransformedData")
    validObject(expected);

    result <- Transformation.normalize(data);
    expect_s4_class(result, "TransformedData")
    validObject(result);

    expect_identical(expected@data, result@data);
    expect_equal(result@transformation@forward(data), expected@data);
    expect_equal(result@transformation@backward(result@data), data);

    expected <- Transformation.apply(data, transformation, TRUE, TRUE);
    expect_s4_class(expected, "TransformedData")
    validObject(expected);

    result <- Transformation.normalizeNegated(data);
    expect_s4_class(result, "TransformedData")
    validObject(result);

    expect_identical(expected@data, result@data);
    expect_equal(result@transformation@forward(data), expected@data);
    expect_equal(result@transformation@backward(result@data), data);
  }
})


test_that("Test Transformation.normalize (III)", {
  data <- c(1, 2, 3, 4);
  transformation <- Transformation.new(identity, identity);

  expected <- Transformation.apply(data, transformation, TRUE, FALSE);
  expect_s4_class(expected, "TransformedData")
  validObject(expected);

  result <- Transformation.normalize(data);
  expect_s4_class(result, "TransformedData")
  validObject(result);

  expect_identical(expected@data, result@data);
  expect_identical(result@transformation@forward(data), expected@data);
  expect_identical(result@transformation@backward(result@data), data);
  expect_identical(result@data, c(0, 1/3, 2/3, 1));

  expected <- Transformation.apply(data, transformation, TRUE, TRUE);
  expect_s4_class(expected, "TransformedData")
  validObject(expected);

  result <- Transformation.normalizeNegated(data);
  expect_s4_class(result, "TransformedData")
  validObject(result);

  expect_identical(expected@data, result@data);
  expect_identical(result@transformation@forward(data), expected@data);
  expect_identical(result@transformation@backward(result@data), data);
  expect_identical(result@data, c(1, 2/3, 1/3, 0));
})


test_that("Test Transformation.normalize (IV)", {
  data <- c(0, 1, 2, 3, 4);
  transformation <- Transformation.new(identity, identity);

  expected <- Transformation.apply(data, transformation, TRUE, FALSE);
  expect_s4_class(expected, "TransformedData")
  validObject(expected);

  result <- Transformation.normalize(data);
  expect_s4_class(result, "TransformedData")
  validObject(result);

  expect_identical(expected@data, result@data);
  expect_identical(result@transformation@forward(data), expected@data);
  expect_identical(result@transformation@backward(result@data), data);
  expect_identical(result@data, c(0, 1/4, 2/4, 3/4, 1));

  expected <- Transformation.apply(data, transformation, TRUE, TRUE);
  expect_s4_class(expected, "TransformedData")
  validObject(expected);

  result <- Transformation.normalizeNegated(data);
  expect_s4_class(result, "TransformedData")
  validObject(result);

  expect_identical(expected@data, result@data);
  expect_identical(result@transformation@forward(data), expected@data);
  expect_identical(result@transformation@backward(result@data), data);
  expect_identical(result@data, c(1, 3/4, 2/4, 1/4, 0));
})