library("dataTransformeR")
context("Transformation")

test_that("Test Transformation constructor (I)", {
  funcs <- new ("Transformation", forward=sin, backward=asin)
  expect_identical(funcs@forward, sin)
  expect_identical(funcs@backward, asin)
  expect_identical(funcs@complexity, 1L)
  expect_s4_class(funcs, "Transformation")
  validObject(funcs)


  funcs <- new ("Transformation", forward=sin, backward=asin, complexity=3L)
  expect_identical(funcs@forward, sin)
  expect_identical(funcs@backward, asin)
  expect_identical(funcs@complexity, 3L)
  expect_s4_class(funcs, "Transformation")
  validObject(funcs)
})

test_that("Test Transformation constructor (II)", {
  f<-function(x) x+5;
  b<-function(x) x-5;
  funcs <- new ("Transformation", forward=f, backward=b)
  expect_s4_class(funcs, "Transformation")
  expect_identical(funcs@forward, f)
  expect_identical(funcs@backward, b)
  expect_identical(funcs@complexity, 1L)
  validObject(funcs)


  funcs <- new ("Transformation", forward=f, backward=b, complexity=4L)
  expect_s4_class(funcs, "Transformation")
  expect_identical(funcs@forward, f)
  expect_identical(funcs@backward, b)
  expect_identical(funcs@complexity, 4L)
  validObject(funcs)
})

test_that("Test Transformation.new (I)", {
  funcs <- Transformation.new(forward=sin, backward=asin)
  expect_identical(funcs@forward, sin)
  expect_identical(funcs@backward, asin)
  expect_identical(funcs@complexity, 1L)
  expect_s4_class(funcs, "Transformation")
  validObject(funcs)

  funcs <- Transformation.new(forward=sin, backward=asin, complexity = 7L)
  expect_identical(funcs@forward, sin)
  expect_identical(funcs@backward, asin)
  expect_identical(funcs@complexity, 7L)
  expect_s4_class(funcs, "Transformation")
  validObject(funcs)
})

test_that("Test Transformation.new (II)", {
  f<-function(x) x+5;
  b<-function(x) x-5;
  funcs <- Transformation.new(forward=f, backward=b)
  expect_equal(funcs@forward, f)
  expect_equal(funcs@backward, b)
  expect_identical(funcs@complexity, 1L)
  expect_s4_class(funcs, "Transformation")
  validObject(funcs)

  funcs <- Transformation.new(forward=f, backward=b, complexity = 2L)
  expect_equal(funcs@forward, f)
  expect_equal(funcs@backward, b)
  expect_identical(funcs@complexity, 2L)
  expect_s4_class(funcs, "Transformation")
  validObject(funcs)
})

test_that("Test Transformation.new error (I)", {
  expect_error(Transformation.new(forward=NULL, backward=NULL))
})

test_that("Test Transformation.new error (II)", {
  expect_error(Transformation.new(forward=sin, backward=NULL))
})

test_that("Test Transformation.new error (III)", {
  expect_error(Transformation.new(forward=NULL, backward=sin))
})

test_that("Test Transformation.new error (IV)", {
  expect_error(Transformation.new(forward=log, backward=NULL))
})

test_that("Test Transformation.new error (V)", {
  expect_error(Transformation.new(forward=sin, backward=log))
})

test_that("Test Transformation.new error (VI)", {
  expect_error(Transformation.new(forward=sin, backward=asin, complexity = 0L))
})

test_that("Test Transformation.new error (VII)", {
  expect_error(Transformation.new(forward=sin, backward=asin, complexity = 3))
})

test_that("Test Transformation.new error (VII)", {
  expect_error(Transformation.new(forward=sin, backward=asin, complexity = -2L))
})

test_that("Test Transformation.new error (VIII)", {
  expect_error(Transformation.new(forward=sin, backward=asin, complexity = c(1L,1L)))
})
