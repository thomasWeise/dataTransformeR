library("dataTransformeR")
context("TransformedData.select")

test_that("Test TransformedData selection on 1D data", {
  data <- c(-1,-2,-3,-4,-5)
  transformation <- Transformation.new(forward=sin, backward=asin);
  transformed <- TransformedData.new(transformation, data);
  expect_identical(transformed@transformation, transformation);
  expect_identical(transformed@data, data);
  validObject(transformed);

  selected <- TransformedData.select(transformed, NULL);
  expect_identical(selected, transformed);
  selected2 <- TransformedData.select.1D(transformed, NULL);
  expect_identical(selected2, selected);

  selected <- TransformedData.select(transformed, c(1));
  expect_identical(selected@transformation, transformed@transformation);
  expect_identical(selected@data, c(-1));
  selected2 <- TransformedData.select.1D(transformed, c(1));
  expect_identical(selected2, selected);

  selected <- TransformedData.select(transformed, c(2));
  expect_identical(selected@transformation, transformed@transformation);
  expect_identical(selected@data, c(-2));
  selected2 <- TransformedData.select.1D(transformed, c(2));
  expect_identical(selected2, selected);

  selected <- TransformedData.select(transformed, c(5));
  expect_identical(selected@transformation, transformed@transformation);
  expect_identical(selected@data, c(-5));
  selected2 <- TransformedData.select.1D(transformed, c(5));
  expect_identical(selected2, selected);

  selected <- TransformedData.select(transformed, c(3, 2, 1));
  expect_identical(selected@transformation, transformed@transformation);
  expect_identical(selected@data, c(-3, -2, -1));
  selected2 <- TransformedData.select.1D(transformed, c(3, 2, 1));
  expect_identical(selected2, selected);

  selected <- TransformedData.select(transformed, c(5, 1, 2));
  expect_identical(selected@transformation, transformed@transformation);
  expect_identical(selected@data, c(-5, -1, -2));
  selected2 <- TransformedData.select.1D(transformed, c(5, 1, 2));
  expect_identical(selected2, selected);

  selected <- TransformedData.select(transformed, c(5, 4, 1, 3, 2));
  expect_identical(selected@transformation, transformed@transformation);
  expect_identical(selected@data, c(-5, -4, -1, -3, -2));
  selected2 <- TransformedData.select.1D(transformed, c(5, 4, 1, 3, 2));
  expect_identical(selected2, selected);
})

test_that("Test TransformedData selection on 1D data", {
  data.x <- c(-1,-2,-3,-4,-5)
  data.y <- c(10,20,30,40,50)
  transformation.x <- Transformation.new(forward=sin, backward=asin);
  transformation.y <- Transformation.new(forward=exp, backward=function(x) log(x));
  transformed.x <- TransformedData.new(transformation.x, data.x);
  validObject(transformed.x);
  transformed.y <- TransformedData.new(transformation.y, data.y);
  validObject(transformed.y);
  transformed <- TransformedData2D.new(transformed.x, transformed.y);
  validObject(transformed);
  expect_identical(transformed@x, transformed.x);
  expect_identical(transformed@y, transformed.y);

  selected <- TransformedData.select(transformed, NULL);
  expect_identical(selected, transformed);
  selected2 <- TransformedData.select.2D(transformed, NULL);
  expect_identical(selected2, selected);

  selected <- TransformedData.select(transformed, c(1));
  expect_identical(selected@x@transformation, transformed.x@transformation);
  expect_identical(selected@x@data, c(-1));
  expect_identical(selected@y@transformation, transformed.y@transformation);
  expect_identical(selected@y@data, c(10));
  selected2 <- TransformedData.select.2D(transformed, c(1));
  expect_identical(selected2, selected);

  selected <- TransformedData.select(transformed, c(2));
  expect_identical(selected@x@transformation, transformed.x@transformation);
  expect_identical(selected@x@data, c(-2));
  expect_identical(selected@y@transformation, transformed.y@transformation);
  expect_identical(selected@y@data, c(20));
  selected2 <- TransformedData.select.2D(transformed, c(2));
  expect_identical(selected2, selected);

  selected <- TransformedData.select(transformed, c(5));
  expect_identical(selected@x@transformation, transformed.x@transformation);
  expect_identical(selected@x@data, c(-5));
  expect_identical(selected@y@transformation, transformed.y@transformation);
  expect_identical(selected@y@data, c(50));
  selected2 <- TransformedData.select.2D(transformed, c(5));
  expect_identical(selected2, selected);

  selected <- TransformedData.select(transformed, c(3, 4));
  expect_identical(selected@x@transformation, transformed.x@transformation);
  expect_identical(selected@x@data, c(-3, -4));
  expect_identical(selected@y@transformation, transformed.y@transformation);
  expect_identical(selected@y@data, c(30, 40));
  selected2 <- TransformedData.select.2D(transformed, c(3, 4));
  expect_identical(selected2, selected);

  selected <- TransformedData.select(transformed, c(4, 2));
  expect_identical(selected@x@transformation, transformed.x@transformation);
  expect_identical(selected@x@data, c(-4, -2));
  expect_identical(selected@y@transformation, transformed.y@transformation);
  expect_identical(selected@y@data, c(40, 20));
  selected2 <- TransformedData.select.2D(transformed, c(4, 2));
  expect_identical(selected2, selected);

  selected <- TransformedData.select(transformed, c(3, 4, 5, 1));
  expect_identical(selected@x@transformation, transformed.x@transformation);
  expect_identical(selected@x@data, c(-3, -4, -5, -1));
  expect_identical(selected@y@transformation, transformed.y@transformation);
  expect_identical(selected@y@data, c(30, 40, 50, 10));
  selected2 <- TransformedData.select.2D(transformed, c(3, 4, 5, 1));
  expect_identical(selected2, selected);

  selected <- TransformedData.select(transformed, c(3, 2, 4, 5, 1));
  expect_identical(selected@x@transformation, transformed.x@transformation);
  expect_identical(selected@x@data, c(-3, -2, -4, -5, -1));
  expect_identical(selected@y@transformation, transformed.y@transformation);
  expect_identical(selected@y@data, c(30, 20, 40, 50, 10));
  selected2 <- TransformedData.select.2D(transformed, c(3, 2, 4, 5, 1));
  expect_identical(selected2, selected);
})
