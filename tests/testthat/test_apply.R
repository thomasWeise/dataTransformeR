library("dataTransformeR")
context("Transformation.apply")


test_that("Test Transformation.apply with identity mapping (I)", {
  for(i in 1:10) {
    data <- rnorm(100);
    data.range <- range(data);
    transformation <- Transformation.new(identity, identity);

    result <- Transformation.apply(data, transformation, FALSE, FALSE);
    expect_s4_class(result, "TransformedData");
    validObject(result);
    validObject(result@transformation);
    expect_identical(result@transformation, transformation);
    expect_identical(result@data, data);
    expect_equal(result@transformation@forward(data), result@data)
    expect_equal(result@transformation@backward(result@data), data)

    result <- Transformation.apply(data, transformation, TRUE, FALSE);
    expect_s4_class(result, "TransformedData");
    validObject(result);
    validObject(result@transformation);

    expect_equal(result@transformation@forward(data.range[1]), 0);
    expect_equal(result@transformation@forward(data.range[2]), 1);
    result.range <- range(result@data);

    expect_equal(result.range[1], 0);
    expect_equal(result.range[2], 1);
    expect_equal(result@transformation@backward(result.range[1]), data.range[1]);
    expect_equal(result@transformation@backward(result.range[2]), data.range[2]);
    expect_equal(result@transformation@forward(data), result@data)
    expect_equal(result@transformation@backward(result@data), data)

    result <- Transformation.apply(data, transformation, TRUE, TRUE);
    expect_s4_class(result, "TransformedData");
    validObject(result);
    validObject(result@transformation);

    expect_equal(result@transformation@forward(data.range[1]), 1);
    expect_equal(result@transformation@forward(data.range[2]), 0);
    result.range <- range(result@data);

    expect_equal(result.range[1], 0);
    expect_equal(result.range[2], 1);
    expect_equal(result@transformation@backward(result.range[1]), data.range[2]);
    expect_equal(result@transformation@backward(result.range[2]), data.range[1]);
    expect_equal(result@transformation@forward(data), result@data)
    expect_equal(result@transformation@backward(result@data), data)
  }
})


test_that("Test Transformation.apply with identity mapping (II)", {
  for(i in 1:10) {
    data <- runif(100);
    data.range <- range(data);
    transformation <- Transformation.new(identity, identity);

    result <- Transformation.apply(data, transformation, FALSE, FALSE);
    expect_s4_class(result, "TransformedData");
    validObject(result);
    validObject(result@transformation);
    expect_identical(result@transformation, transformation);
    expect_identical(result@data, data);
    expect_equal(result@transformation@forward(data), result@data)
    expect_equal(result@transformation@backward(result@data), data)

    result <- Transformation.apply(data, transformation, TRUE, FALSE);
    expect_s4_class(result, "TransformedData");
    validObject(result);
    validObject(result@transformation);

    expect_equal(result@transformation@forward(data.range[1]), 0);
    expect_equal(result@transformation@forward(data.range[2]), 1);
    result.range <- range(result@data);

    expect_equal(result.range[1], 0);
    expect_equal(result.range[2], 1);
    expect_equal(result@transformation@backward(result.range[1]), data.range[1]);
    expect_equal(result@transformation@backward(result.range[2]), data.range[2]);
    expect_equal(result@transformation@forward(data), result@data)
    expect_equal(result@transformation@backward(result@data), data)

    result <- Transformation.apply(data, transformation, TRUE, TRUE);
    expect_s4_class(result, "TransformedData");
    validObject(result);
    validObject(result@transformation);

    expect_equal(result@transformation@forward(data.range[1]), 1);
    expect_equal(result@transformation@forward(data.range[2]), 0);
    result.range <- range(result@data);

    expect_equal(result.range[1], 0);
    expect_equal(result.range[2], 1);
    expect_equal(result@transformation@backward(result.range[1]), data.range[2]);
    expect_equal(result@transformation@backward(result.range[2]), data.range[1]);
    expect_equal(result@transformation@forward(data), result@data)
    expect_equal(result@transformation@backward(result@data), data)
  }
})


test_that("Test Transformation.apply with identity mapping (III)", {
  data <- c(1, 2, 3, 4);
  transformation <- Transformation.new(identity, identity);

  result <- Transformation.apply(data, transformation, FALSE, FALSE);
  expect_s4_class(result, "TransformedData");
  validObject(result);
  validObject(result@transformation);
  expect_identical(result@transformation, transformation);
  expect_identical(result@data, data);
  expect_equal(result@transformation@forward(data), result@data)
  expect_equal(result@transformation@backward(result@data), data)

  result <- Transformation.apply(data, transformation, TRUE, FALSE);
  expect_s4_class(result, "TransformedData");
  validObject(result);
  validObject(result@transformation);
  expect_equal(result@transformation@forward(data), result@data)
  expect_equal(result@transformation@backward(result@data), data)
  expect_equal(result@data, c(0, 1/3, 2/3, 1))

  result <- Transformation.apply(data, transformation, TRUE, TRUE);
  expect_s4_class(result, "TransformedData");
  validObject(result);
  validObject(result@transformation);
  expect_equal(result@transformation@forward(data), result@data)
  expect_equal(result@transformation@backward(result@data), data)
  expect_equal(result@data, c(1, 2/3, 1/3, 0))
})


test_that("Test Transformation.apply with identity mapping (IV)", {
  data <- c(0, 1, 2, 3, 4);
  transformation <- Transformation.new(identity, identity);

  result <- Transformation.apply(data, transformation, FALSE, FALSE);
  expect_s4_class(result, "TransformedData");
  validObject(result);
  validObject(result@transformation);
  expect_identical(result@transformation, transformation);
  expect_identical(result@data, data);
  expect_equal(result@transformation@forward(data), result@data)
  expect_equal(result@transformation@backward(result@data), data)

  result <- Transformation.apply(data, transformation, TRUE, FALSE);
  expect_s4_class(result, "TransformedData");
  validObject(result);
  validObject(result@transformation);
  expect_equal(result@transformation@forward(data), result@data)
  expect_equal(result@transformation@backward(result@data), data)
  expect_equal(result@data, c(0, 1/4, 2/4, 3/4, 1))

  result <- Transformation.apply(data, transformation, TRUE, TRUE);
  expect_s4_class(result, "TransformedData");
  validObject(result);
  validObject(result@transformation);
  expect_equal(result@transformation@forward(data), result@data)
  expect_equal(result@transformation@backward(result@data), data)
  expect_equal(result@data, c(1, 3/4, 2/4, 1/4, 0))
})

test_that("Test Transformation.apply with logarithmic mapping (I)", {
  for(i in 1:10) {
    data <- abs(rnorm(100)) + 1;
    data.range <- range(data);
    transformation <- Transformation.new(function(x) log(x), exp);

    result <- Transformation.apply(data, transformation, FALSE, FALSE);
    expect_s4_class(result, "TransformedData");
    validObject(result);
    validObject(result@transformation);
    expect_identical(result@transformation, transformation);
    expect_identical(result@data, log(data));
    expect_equal(result@transformation@forward(data), result@data)
    expect_equal(result@transformation@backward(result@data), data)

    result <- Transformation.apply(data, transformation, TRUE, FALSE);
    expect_s4_class(result, "TransformedData");
    validObject(result);
    validObject(result@transformation);
    expect_equal(result@transformation@forward(data.range[1]), 0);
    expect_equal(result@transformation@forward(data.range[2]), 1);
    result.range <- range(result@data);
    expect_equal(result.range[1], 0);
    expect_equal(result.range[2], 1);
    expect_equal(result@transformation@backward(result.range[1]), data.range[1]);
    expect_equal(result@transformation@backward(result.range[2]), data.range[2]);
    expect_equal(result@transformation@forward(data), result@data)
    expect_equal(result@transformation@backward(result@data), data)

    result <- Transformation.apply(data, transformation, TRUE, TRUE);
    expect_s4_class(result, "TransformedData");
    validObject(result);
    validObject(result@transformation);
    expect_equal(result@transformation@forward(data.range[1]), 1);
    expect_equal(result@transformation@forward(data.range[2]), 0);
    result.range <- range(result@data);
    expect_equal(result.range[1], 0);
    expect_equal(result.range[2], 1);
    expect_equal(result@transformation@backward(result.range[1]), data.range[2]);
    expect_equal(result@transformation@backward(result.range[2]), data.range[1]);
    expect_equal(result@transformation@forward(data), result@data)
    expect_equal(result@transformation@backward(result@data), data)
  }
})


test_that("Test Transformation.apply with logarithmic mapping (II)", {
  for(i in 1:10) {
    data <- abs(runif(100)) + 1e-10;
    data.range <- range(data);
    transformation <- Transformation.new(function(x) log(x), exp);

    result <- Transformation.apply(data, transformation, FALSE, FALSE);
    expect_s4_class(result, "TransformedData");
    validObject(result);
    validObject(result@transformation);
    expect_identical(result@transformation, transformation);
    expect_identical(result@data, log(data));
    expect_equal(result@transformation@forward(data), result@data)
    expect_equal(result@transformation@backward(result@data), data)

    result <- Transformation.apply(data, transformation, TRUE, FALSE);
    expect_s4_class(result, "TransformedData");
    validObject(result);
    validObject(result@transformation);
    expect_equal(result@transformation@forward(data.range[1]), 0);
    expect_equal(result@transformation@forward(data.range[2]), 1);
    result.range <- range(result@data);
    expect_equal(result.range[1], 0);
    expect_equal(result.range[2], 1);
    expect_equal(result@transformation@backward(result.range[1]), data.range[1]);
    expect_equal(result@transformation@backward(result.range[2]), data.range[2]);
    expect_equal(result@transformation@forward(data), result@data)
    expect_equal(result@transformation@backward(result@data), data)

    result <- Transformation.apply(data, transformation, TRUE, TRUE);
    expect_s4_class(result, "TransformedData");
    validObject(result);
    validObject(result@transformation);
    expect_equal(result@transformation@forward(data.range[1]), 1);
    expect_equal(result@transformation@forward(data.range[2]), 0);
    result.range <- range(result@data);
    expect_equal(result.range[1], 0);
    expect_equal(result.range[2], 1);
    expect_equal(result@transformation@backward(result.range[1]), data.range[2]);
    expect_equal(result@transformation@backward(result.range[2]), data.range[1]);
    expect_equal(result@transformation@forward(data), result@data)
    expect_equal(result@transformation@backward(result@data), data)
  }
})


test_that("Test Transformation.apply with logarithmic mapping (III)", {
  data <- c(1, 2, 3, 4);
  transformation <- Transformation.new(function(x) log(x), exp);

  result <- Transformation.apply(data, transformation, FALSE, FALSE);
  expect_s4_class(result, "TransformedData");
  validObject(result);
  validObject(result@transformation);
  expect_identical(result@transformation, transformation);
  expect_identical(result@data, log(data));
  expect_equal(result@transformation@forward(data), result@data)
  expect_equal(result@transformation@backward(result@data), data)

  result <- Transformation.apply(data, transformation, TRUE, FALSE);
  expect_s4_class(result, "TransformedData");
  validObject(result);
  validObject(result@transformation);
  expect_equal(result@transformation@forward(data), result@data)
  expect_equal(result@transformation@backward(result@data), data)
  log.min <- log(1);
  log.max <- log(4);
  log.range <- log.max - log.min;
  expect_equal(result@data, c((log(1)-log.min)/log.range,
                              (log(2)-log.min)/log.range,
                              (log(3)-log.min)/log.range,
                              (log(4)-log.min)/log.range))

  result <- Transformation.apply(data, transformation, TRUE, TRUE);
  expect_s4_class(result, "TransformedData");
  validObject(result);
  validObject(result@transformation);
  expect_equal(result@transformation@forward(data), result@data)
  expect_equal(result@transformation@backward(result@data), data)
  expect_equal(result@data, c(1,
                              1-(log(2)-log.min)/log.range,
                              1-(log(3)-log.min)/log.range,
                              0))
})


test_that("Test Transformation.apply impossible", {
  data <- c(1, 2, NaN, 4);
  transformation <- Transformation.new(identity, identity);
  expect_null(Transformation.apply(data, transformation, FALSE, FALSE));

  data <- c(1, +Inf, 5, 4);
  transformation <- Transformation.new(identity, identity);
  expect_null(Transformation.apply(data, transformation, FALSE, FALSE));

  data <- c(-Inf, 2, 5, 4);
  transformation <- Transformation.new(identity, identity);
  expect_null(Transformation.apply(data, transformation, FALSE, FALSE));

  data <- c(1, 2, NA, 4);
  transformation <- Transformation.new(identity, identity);
  expect_null(Transformation.apply(data, transformation, FALSE, FALSE));
})

test_that("Test Transformation.apply collapse", {
  data <- c(1, 1, 1, 1);
  transformation <- Transformation.new(identity, identity);
  result <- Transformation.apply(data, transformation, TRUE, FALSE);
  expect_identical(result@data, rep(0.5, 4));
  expect_identical(result@transformation@forward(data), result@data);
  expect_identical(result@transformation@backward(result@data), rep(1, 4));

  data <- c(1, 5, 3, 2);
  transformation <- Transformation.new(function(x) 5, function(x) 5);
  result <- Transformation.apply(data, transformation, TRUE, FALSE);
  expect_identical(result@data, rep(0.5, 4));
  expect_identical(result@transformation@forward(data), result@data);
  expect_identical(result@transformation@backward(result@data), rep(3, 4));
})