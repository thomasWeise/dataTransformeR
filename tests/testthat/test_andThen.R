library("dataTransformeR")
context("Transformation.andThen")


test_that("Test Transformation.andThen1", {
  logFun <- function(x) log(x)
  funcs1 <- new ("Transformation", forward=exp, backward=logFun)
  funcs2 <- new ("Transformation", forward=logFun, backward=exp, complexity = 2L)
  funcs3 <- Transformation.andThen1(funcs1, funcs2)
  funcs4 <- new ("Transformation", forward=function(x){x+3}, backward=function(x){x-3},
                                   complexity = 5L)
  funcs5 <- Transformation.andThen1(funcs1, funcs4)
  funcs6 <- new ("Transformation", forward=function(x){x*3}, backward=function(x){x/3},
                                   complexity = 21L)
  funcs7 <- Transformation.andThen1(funcs5, funcs6)

  validObject(funcs1)
  validObject(funcs2)
  validObject(funcs3)
  expect_identical(funcs1@forward, exp)
  expect_equal(funcs1@backward, logFun)
  expect_equal(funcs2@forward, logFun)
  expect_identical(funcs2@backward, exp)
  expect_identical(funcs3@forward(2), 2)
  expect_identical(funcs3@backward(2), 2)
  expect_identical(funcs3@complexity, 1L + 2L)
  expect_identical(funcs5@forward(2), exp(2)+3)
  expect_identical(funcs5@backward(5), log(5-3))
  expect_identical(funcs7@forward(2), 3*(exp(2)+3))
  expect_identical(funcs7@backward(12), log((12/3)-3))
  expect_identical(funcs5@complexity, 1L + 5L)
  expect_identical(funcs7@complexity, 1L + 5L + 21L)
  validObject(funcs4)
  validObject(funcs5)
  validObject(funcs6)
  validObject(funcs7)
  validObject(funcs4)
})

test_that("Test Transformation.andThen2", {
  funcs1 <- new ("Transformation", forward=sin, backward=asin)
  funcs2 <- Transformation.andThen2(funcs1, cos, acos)
  validObject(funcs2)
  funcs3 <- Transformation.andThen2(funcs2, function(x){x*x}, sqrt)

  expect_identical(funcs1@forward, sin)
  expect_identical(funcs1@backward, asin)
  x<-cos(sin(0.5))
  expect_identical(funcs2@forward(0.5), x)
  expect_equal(funcs2@backward(x), 0.5)
  y<-x*x
  expect_identical(funcs3@forward(0.5), y)
  expect_equal(funcs3@backward(y), 0.5)

  validObject(funcs3)
  validObject(funcs1)
})
