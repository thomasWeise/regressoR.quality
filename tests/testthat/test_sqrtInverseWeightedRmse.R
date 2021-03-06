library("regressoR.quality")
context("RegressionQualityMetric.sqrtInverseWeightedRmse")

test_that("Test RegressionQualityMetric.sqrtInverseWeightedRmse", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17);
  f <- function(x, par) { par[1] + (par[2] * x) + (par[3] * (x^2)) + (par[4] * (x^3)) };
  origpar <- c(2, 0.2, -0.1, 0.9);
  g <- function(x) { f(x, origpar) };
  gradient <- function(x, par) { c(1, x, x^2, x^3) };
  y <- g(x);

  rmse <- RegressionQualityMetric.sqrtInverseWeightedRmse(x, y);
  validObject(rmse)
  expect_equal(is.null(rmse@quality), FALSE)
  expect_equal(is.null(rmse@residuals), FALSE)
  expect_equal(is.null(rmse@jacobian), FALSE)
  expect_equal(is.null(rmse@x), FALSE)
  expect_identical(rmse@x, x)
  expect_equal(is.null(rmse@y), FALSE)
  expect_identical(rmse@y, y)
  expect_equal(is.null(rmse@weights), FALSE)

  expect_equal(rmse@quality(f, origpar), 0)
  testpar <- c(1, 0.3, 0.2, -0.7);
  residuals <- rmse@residuals(f, testpar)
  expect_identical(is.vector(residuals), TRUE)
  expect_identical(length(residuals), length(x))
  for( i in 1:length(x) ) {
    expect_equal( (y[i] - f(x[i], testpar)) / sqrt(abs(y[i])), residuals[i])
  }
  expect_equal(rmse@quality(f, testpar), sqrt(mean(residuals^2)))

  jacobian <- rmse@jacobian(gradient, testpar)
  expect_equal(is.matrix(jacobian), TRUE)
  expect_equal(dim(jacobian), c(length(x),length(testpar)))
  i <- 0
  for(j in 1:length(testpar)) {
    for(k in 1:length(x)) {
      i <- i + 1;
      expect_equal(jacobian[i], -gradient(x[k], testpar)[j]/sqrt(abs(y[k])))
    }
  }

  if(require(minpack.lm)) {
    fn <- function(par) { rmse@residuals(f, par); };
    jac <- function(par) { rmse@jacobian(gradient, par); };
    result <- nls.lm(par=testpar, fn=fn, jac=jac);
    expect_equal( (result$deviance < 1e-10), TRUE)
    expect_equal( (sqrt(mean((origpar - result$par)^2)) < 1e-10), TRUE)
  }
})





test_that("Test sqrt-inverse weighted rmse (2)", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17);
  f <- function(x, par) { sin(par[1] + (x*par[2])) };
  origpar <- c(2, 0.2);
  g <- function(x) f(x, origpar);
  gradient <- function(x, par) {z <- cos(par[2]*x+par[1]); c(z, x*z) };
  y <- g(x);

  rmse <- RegressionQualityMetric.sqrtInverseWeightedRmse(x, y);
  validObject(rmse)
  expect_equal(is.null(rmse@quality), FALSE)
  expect_equal(is.null(rmse@residuals), FALSE)
  expect_equal(is.null(rmse@jacobian), FALSE)
  expect_equal(is.null(rmse@x), FALSE)
  expect_identical(rmse@x, x)
  expect_equal(is.null(rmse@y), FALSE)
  expect_identical(rmse@y, y)
  expect_equal(is.null(rmse@weights), FALSE)

  expect_equal(rmse@quality(f, origpar), 0)
  testpar <- c(1, 0.3);
  residuals <- rmse@residuals(f, testpar)
  expect_identical(is.vector(residuals), TRUE)
  expect_identical(length(residuals), length(x))
  for( i in 1:length(x) ) {
    expect_equal( (y[i] - f(x[i], testpar)) / sqrt(abs(y[i])), residuals[i])
  }
  expect_equal(rmse@quality(f, testpar), sqrt(mean(residuals^2)))

  jacobian <- rmse@jacobian(gradient, testpar)
  expect_equal(is.matrix(jacobian), TRUE)
  expect_equal(dim(jacobian), c(length(x),length(testpar)))
  i <- 0
  for(j in 1:length(testpar)) {
    for(k in 1:length(x)) {
      i <- i + 1;
      expect_equal(jacobian[i], -gradient(x[k], testpar)[j]/sqrt(abs(y[k])))
    }
  }

  if(require(minpack.lm)) {
    fn <- function(par) { rmse@residuals(f, par); };
    jac <- function(par) { rmse@jacobian(gradient, par); };
    result <- nls.lm(par=testpar, fn=fn, jac=jac);
    expect_equal( (result$deviance < 1e-10), TRUE)
    expect_equal( (sqrt(mean((origpar - result$par)^2)) < 1e-10), TRUE)
  }
})


test_that("Test RegressionQualityMetric.sqrtInverseWeightedRmse", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17);
  f <- function(x, par) { par[1] + (par[2] * x) + (par[3] * (x^2)) + (par[4] * (x^3)) };
  par <- c(-4, 3, -2, 2);
  y <- f(x, par);
  metric <- RegressionQualityMetric.sqrtInverseWeightedRmse(x, y);

  ft <- function(x) f(x, par)
  expect_equal(metric@quality(ft), 0)
  expect_equal(metric@residuals(ft), rep(0, length(y)))

  ft <- function(x) f(x, par)+1
  expect_equal(metric@quality(ft), sqrt(mean(1/abs(y))))
  expect_equal(metric@residuals(ft), -1/sqrt(abs(y)))

  ft <- function(x) f(x, par)+2
  expect_equal(metric@quality(ft), sqrt(mean(4/(abs(y)))))
  expect_equal(metric@residuals(ft), -2/sqrt(abs(y)))
})
