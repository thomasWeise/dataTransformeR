library("dataTransformeR")
context("TransformedData2D")

test_that("Test TransformedData2D constructor (I)", {
  vec1 <- c(1,2,3)
  vec2 <- c(3,3,3)
  data <- new("TransformedData2D",
            x=new ("TransformedData",
               transformation=new("Transformation", forward=sin, backward=asin),
               data=vec1),
            y=new ("TransformedData",
                   transformation=new("Transformation", forward=cos, backward=acos),
                   data=vec2));
  expect_identical(data@x@transformation@forward, sin)
  expect_identical(data@x@transformation@backward, asin)
  expect_identical(data@x@data, vec1)
  expect_identical(data@y@transformation@forward, cos)
  expect_identical(data@y@transformation@backward, acos)
  expect_identical(data@y@data, vec2)
  expect_s4_class(data, "TransformedData2D")
  validObject(data)
})

test_that("Test TransformedData2D constructor (II)", {
  vec1 <- c(1,2,3)
  vec2 <- c(3,3,3)
  data <- new("TransformedData2D",
              x=TransformedData.new(
                     transformation=Transformation.new(forward=sin, backward=asin),
                     data=vec1),
              y=TransformedData.new(
                     transformation=Transformation.new(forward=cos, backward=acos),
                     data=vec2));
  expect_identical(data@x@transformation@forward, sin)
  expect_identical(data@x@transformation@backward, asin)
  expect_identical(data@x@data, vec1)
  expect_identical(data@y@transformation@forward, cos)
  expect_identical(data@y@transformation@backward, acos)
  expect_identical(data@y@data, vec2)
  expect_s4_class(data, "TransformedData2D")
  validObject(data)
})



test_that("Test TransformedData2D.new (I)", {
  vec1 <- c(1,2,3)
  vec2 <- c(3,3,3)
  data <- TransformedData2D.new(
              x=new ("TransformedData",
                     transformation=new("Transformation", forward=sin, backward=asin),
                     data=vec1),
              y=new ("TransformedData",
                     transformation=new("Transformation", forward=cos, backward=acos),
                     data=vec2));
  expect_identical(data@x@transformation@forward, sin)
  expect_identical(data@x@transformation@backward, asin)
  expect_identical(data@x@data, vec1)
  expect_identical(data@y@transformation@forward, cos)
  expect_identical(data@y@transformation@backward, acos)
  expect_identical(data@y@data, vec2)
  expect_s4_class(data, "TransformedData2D")
  validObject(data)
})

test_that("Test TransformedData2D.new (II)", {
  vec1 <- c(1,2,3)
  vec2 <- c(3,3,3)
  data <- TransformedData2D.new(
              x=TransformedData.new(
                transformation=Transformation.new(forward=sin, backward=asin),
                data=vec1),
              y=TransformedData.new(
                transformation=Transformation.new(forward=cos, backward=acos),
                data=vec2));
  expect_identical(data@x@transformation@forward, sin)
  expect_identical(data@x@transformation@backward, asin)
  expect_identical(data@x@data, vec1)
  expect_identical(data@y@transformation@forward, cos)
  expect_identical(data@y@transformation@backward, acos)
  expect_identical(data@y@data, vec2)
  expect_s4_class(data, "TransformedData2D")
  validObject(data)
})

test_that("Test TransformedData2D.new (III)", {
  vec <- c(1,1,2)
  expect_error(TransformedData2D.new(
    x=NULL,
    y=TransformedData.new(
      transformation=Transformation.new(forward=cos, backward=acos),
      data=vec)))
})


test_that("Test TransformedData2D.new (IV)", {
  vec <- c(1,1,2)
  expect_error(TransformedData2D.new(
    x=TransformedData.new(
      transformation=Transformation.new(forward=cos, backward=acos),
      data=vec)),
    y=NULL)
})


test_that("Test TransformedData2D.new (V)", {
  expect_error(TransformedData2D.new(
    x=NULL,
    y=NULL))
})
