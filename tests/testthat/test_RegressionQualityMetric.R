library("regressoR.quality")
context("RegressionQualityMetric")

test_that("Test RegressionQualityMetric constructor", {
  x <- c(1, 2, 3);
  y <- log(x);
  quality <- function(f, ...) {
    sum <- 0;
    for(i in 1:length(x)){
      sum <- (sum + abs(f(x[i], ...) - y[i]));
    }
    return(sum);
  };
  residuals <- function(f, ...) {
    return(output - f(x, ...));
  };
  instance <- new("RegressionQualityMetric", quality=quality, x=x, y=y, residuals=residuals);
  methods::validObject(instance);
  expect_identical(instance@quality, quality);
  expect_identical(instance@x, x);
  expect_identical(instance@y, y);
  expect_identical(instance@residuals, residuals);
  expect_null(instance@jacobian);
})



test_that("Test RegressionQualityMetric constructor", {
  x <- c(1, 2);
  y <- c(0, 0);
  qf <- function(f, ...) (f(1, ...))^2 + (f(0, ...))^2;
  rf <- function(f, ...) c(f(1, ...), f(0, ...));
  jf <- function(gradient, ...) c(gradient(1, ...), gradient(0, ...));
  qm <- new ("RegressionQualityMetric", x=x, y=y, quality=qf, residuals=rf, jacobian=jf);
  expect_identical(qm@x, x)
  expect_identical(qm@y, y)
  expect_identical(qm@quality, qf)
  expect_identical(qm@residuals, rf)
  expect_identical(qm@jacobian, jf)
  methods::validObject(qm)

  qm <- new ("RegressionQualityMetric",
             x=x,
             y=y,
             quality=qf,
             residuals=rf);
  expect_identical(qm@x, x)
  expect_identical(qm@y, y)
  expect_identical(qm@quality, qf)
  expect_identical(qm@residuals, rf)
  expect_identical(is.null(qm@jacobian), TRUE)
  methods::validObject(qm)

  qm <- new ("RegressionQualityMetric",
             x=x,
             y=y,
             quality=qf);
  expect_identical(qm@x, x)
  expect_identical(qm@y, y)
  expect_identical(qm@quality, qf)
  expect_identical(is.null(qm@residuals), TRUE)
  expect_identical(is.null(qm@jacobian), TRUE)
  validObject(qm)

  qm <- new ("RegressionQualityMetric");
  expect_error(validObject(qm))

  expect_error(new ("RegressionQualityMetric", x=x));
  expect_error(new ("RegressionQualityMetric", y=y));
  expect_error(new ("RegressionQualityMetric", quality=quality));
  expect_error(new ("RegressionQualityMetric", quality=quality, x=x));
  expect_error(new ("RegressionQualityMetric", quality=quality, y=y));
  expect_error(new ("RegressionQualityMetric", x=x, y=y));
  expect_error(new ("RegressionQualityMetric",
             x=x,
             y=y,
             quality=qf, jacobian=jf));
})


test_that("Test new", {
  x <- c(1, 2, 3);
  y <- log(x);
  quality <- function(f, ...) {
    sum <- 0;
    for(i in 1:length(x)){
      sum <- (sum + abs(f(x[i], ...) - y[i]));
    }
    return(sum);
  };
  residuals <- function(f, ...) {
    return(output - f(x, ...));
  };
  instance <- RegressionQualityMetric.new(quality=quality,
                  x=x,
                  y=y,
                  residuals=residuals);
  methods::validObject(instance);
  expect_identical(instance@quality, quality);
  expect_identical(instance@x, x);
  expect_identical(instance@y, y);
  expect_identical(instance@residuals, residuals);
  expect_null(instance@jacobian);
})



test_that("Test new", {
  x <- c(1, 2);
  y <- c(0, 0);
  qf <- function(f, ...) (f(1, ...))^2 + (f(0, ...))^2;
  rf <- function(f, ...) c(f(1, ...), f(0, ...));
  jf <- function(gradient, ...) c(gradient(1, ...), gradient(0, ...));
  qm <- RegressionQualityMetric.new(
             x=x,
             y=y,
             quality=qf,
             residuals=rf,
             jacobian=jf);
  expect_identical(qm@x, x)
  expect_identical(qm@y, y)
  expect_identical(qm@quality, qf)
  expect_identical(qm@residuals, rf)
  expect_identical(qm@jacobian, jf)
  methods::validObject(qm)

  qm <- RegressionQualityMetric.new(
             x=x,
             y=y,
             quality=qf,
             residuals=rf);
  expect_identical(qm@x, x)
  expect_identical(qm@y, y)
  expect_identical(qm@quality, qf)
  expect_identical(qm@residuals, rf)
  expect_identical(is.null(qm@jacobian), TRUE)
  methods::validObject(qm)

  qm <- RegressionQualityMetric.new(
             x=x,
             y=y,
             quality=qf);
  expect_identical(qm@x, x)
  expect_identical(qm@y, y)
  expect_identical(qm@quality, qf)
  expect_identical(is.null(qm@residuals), TRUE)
  expect_identical(is.null(qm@jacobian), TRUE)
  validObject(qm)

  expect_error(RegressionQualityMetric.new());
  expect_error(RegressionQualityMetric.new(x=x));
  expect_error(RegressionQualityMetric.new(y=y));
  expect_error(RegressionQualityMetric.new(quality=quality));
  expect_error(RegressionQualityMetric.new(quality=quality, x=x));
  expect_error(RegressionQualityMetric.new(quality=quality, y=y));
  expect_error(RegressionQualityMetric.new(x=x, y=y));
  expect_error(RegressionQualityMetric.new(x=x, y=y, quality=qf, jacobian=jf));
})
