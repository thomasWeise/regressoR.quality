library("regressoR.quality")
context(".fixInverseAbsoluteWeights")

test_that("Test .fixInverseAbsoluteWeights for random positive weights", {
  weights = 0.01 + runif(1000);
  expect_identical(.fixInverseAbsoluteWeights(weights), 1/weights)
})

test_that("Test .fixInverseAbsoluteWeights for positive weights", {
  weights = c(1, 2, 3, 4)
  expect_identical(.fixInverseAbsoluteWeights(weights), 1/weights)
})

test_that("Test .fixInverseAbsoluteWeights for positive weights with zero", {
  weights = c(1, 2, 0, 3, 4)
  expect_identical(.fixInverseAbsoluteWeights(weights),
          c(1, 0.5,  3/(4*4), 1/3, 1/4))
})

test_that("Test .fixInverseAbsoluteWeights for positive weights with zero", {
  weights = c(0, 10, 0, 0, 10)
  expect_identical(.fixInverseAbsoluteWeights(weights),
                   c(0.8/10, 1/10, 0.8/10, 0.8/10, 1/10))
})

test_that("Test .fixInverseAbsoluteWeights for positive weights with zero", {
  weights = c(0, .Machine$double.xmax, 0, 0, .Machine$double.xmax)
  expect_identical(.fixInverseAbsoluteWeights(weights),
                   c(0.8/.Machine$double.xmax, 1/.Machine$double.xmax, 0.8/.Machine$double.xmax, 0.8/.Machine$double.xmax, 1/.Machine$double.xmax))
})

test_that("Test .fixInverseAbsoluteWeights for zero weights", {
  expect_null(.fixInverseAbsoluteWeights(c(0, 0)))
})


