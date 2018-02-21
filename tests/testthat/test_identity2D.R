library("dataTransformeR")
context("Transformation.identity2D")


test_that("Test Transformation.identity2D", {
  x <- c(1, 2, 3)
  y <- c(1, 2, 3)
  result <- Transformation.identity2D(x, y);
  expect_s4_class(result, "TransformedData2D");
  expect_identical(result@x@data, x);
  expect_s4_class(result@x@transformation, "Transformation")
  expect_identical(result@x@transformation@forward, identity);
  expect_identical(result@x@transformation@backward, identity);
  expect_identical(result@y@data, y);
  expect_s4_class(result@y@transformation, "Transformation")
  expect_identical(result@y@transformation@forward, identity);
  expect_identical(result@y@transformation@backward, identity);
})

test_that("Test Transformation.identity2D error", {
  expect_error(Transformation.identity2D(NULL, c(1,2,3)));
})

test_that("Test Transformation.identity2D error", {
  expect_error(Transformation.identity2D(c(1,2,3), NULL));
})

test_that("Test Transformation.identity2D error", {
  expect_error(Transformation.identity2D(NULL, NULL));
})
