library("dataTransformeR")
context("Transformation.identity")


test_that("Test Transformation.identity", {
  data <- c(1, 2, 3)
  result <- Transformation.identity(data);
  expect_s4_class(result, "TransformedData");
  expect_identical(result@data, data);
  expect_s4_class(result@transformation, "Transformation")
  expect_identical(result@transformation@forward, identity);
  expect_identical(result@transformation@backward, identity);
})

test_that("Test Transformation.identity error", {
  expect_error(Transformation.identity(NULL));
})
