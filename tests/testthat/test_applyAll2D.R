library("dataTransformeR")
context("Transformation.applyAll2D")

test_that("Test Transformation.applyAll2D with identity mapping", {
  x <- c(1, 2, 3);
  y <- c(2, 3, 5);
  result.1 <- Transformation.applyAll2D(x, y, x.transformations=c(Transformation.normalize), x.addIdentity=FALSE);
  expect_length(result.1, 1);
  expect_s4_class(result.1[[1]], "TransformedData2D")
  expect_identical(result.1[[1]]@x@data, c(0, 0.5, 1));
  expect_identical(result.1[[1]]@y@data, c(0, 1/3, 1));

  result.2 <- Transformation.applyAll2D(x, y, x.transformations=c(Transformation.normalize), x.addIdentity=TRUE);
  expect_length(result.2, 4);
  expect_s4_class(result.2[[1]], "TransformedData2D")
  expect_s4_class(result.2[[2]], "TransformedData2D")
  expect_s4_class(result.2[[3]], "TransformedData2D")
  expect_s4_class(result.2[[4]], "TransformedData2D")

  x.identity <- FALSE; y.identity <- FALSE; xy.identity <- FALSE; other <- FALSE;
  for(data in result.2) {
    if(identical(data@x@transformation@forward, identity)) {
      if(identical(data@y@transformation@forward, identity)) {
        xy.identity <- TRUE;
        expect_identical(data@x@data, x);
        expect_identical(data@y@data, y);
      } else {
        x.identity <- TRUE;
        expect_identical(data@x@data, x);
        expect_identical(data@y@data, c(0, 1/3, 1));
      }
    } else {
      if(identical(data@y@transformation@forward, identity)) {
        y.identity <- TRUE;
        expect_identical(data@x@data, c(0, 0.5, 1));
        expect_identical(data@y@data, y);
      } else {
        other <- TRUE;
        expect_identical(data@x@data, c(0, 0.5, 1));
        expect_identical(data@y@data, c(0, 1/3, 1));
      }
    }
  }

  expect_true(xy.identity && x.identity && y.identity && other);
})
