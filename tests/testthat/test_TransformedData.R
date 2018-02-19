library("dataTransformeR")
context("TransformedData")

test_that("Test TransformedData constructor (I)", {
  vec <- c(1,2,3)
  data <- new ("TransformedData",
               transformation=new("Transformation", forward=sin, backward=asin),
               data=vec)
  expect_identical(data@transformation@forward, sin)
  expect_identical(data@transformation@backward, asin)
  expect_identical(data@data, vec)
  expect_s4_class(data, "TransformedData")
  validObject(data)
})

test_that("Test TransformedData constructor (II)", {
  vec <- c(1,2,3)
  data <- new ("TransformedData",
               transformation=Transformation.new(forward=sin, backward=asin),
               data=vec)
  expect_identical(data@transformation@forward, sin)
  expect_identical(data@transformation@backward, asin)
  expect_identical(data@data, vec)
  expect_s4_class(data, "TransformedData")
  validObject(data)
})



test_that("Test TransformedData.new (I)", {
  vec <- c(1,2,3)
  data <- TransformedData.new(
               transformation=new("Transformation", forward=sin, backward=asin),
               data=vec)
  expect_identical(data@transformation@forward, sin)
  expect_identical(data@transformation@backward, asin)
  expect_identical(data@data, vec)
  expect_s4_class(data, "TransformedData")
  validObject(data)
})

test_that("Test TransformedData.new (II)", {
  vec <- c(1,2,3)
  data <- TransformedData.new(
               transformation=Transformation.new(forward=sin, backward=asin),
               data=vec)
  expect_identical(data@transformation@forward, sin)
  expect_identical(data@transformation@backward, asin)
  expect_identical(data@data, vec)
  expect_s4_class(data, "TransformedData")
  validObject(data)
})

test_that("Test TransformedData.new (III)", {
  vec <- c()
  expect_error(TransformedData.new(
    transformation=Transformation.new(forward=sin, backward=asin),
    data=vec))
})


test_that("Test TransformedData.new (IV)", {
  vec <- c()
  expect_error(TransformedData.new(
    transformation=Transformation.new(forward=sin, backward=asin),
    data=NULL))
})


test_that("Test TransformedData.new (V)", {
  vec <- c(1,2,3)
  expect_error(TransformedData.new(
    transformation=Transformation.new(forward=NULL, backward=asin),
    data=vec))
})


test_that("Test TransformedData.new (VI)", {
  vec <- c(1,2,3)
  expect_error(TransformedData.new(
    transformation=NULL,
    data=vec))
})