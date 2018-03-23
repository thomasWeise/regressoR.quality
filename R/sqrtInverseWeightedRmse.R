#' @include weightedRmse.R
#' @include fixInverseAbsoluteWeights.R

#' @title Create a Root-Mean Square Error Quality RegressionQualityMetric where
#' the Weights are the Inverse of the Square Root of the Absolute \code{y}
#' Values
#'
#' @description Create a model quality metric based on the root mean square
#' error, using the inverse of the square roots absolute \code{y} values as
#' weights.
#'
#'
#' @param x the input coordinates
#' @param y the output coordinates
#' @return a \code{\link{RegressionQualityMetric}} based on the RMSE weighted by
#'   the inverse absolute \code{y} values.
#' @export RegressionQualityMetric.sqrtInverseWeightedRmse
#' @seealso RegressionQualityMetric.new
#' @seealso RegressionQualityMetric.weightedRmse
#' @seealso RegressionQualityMetric.inverseWeightedRmse
RegressionQualityMetric.sqrtInverseWeightedRmse <- function(x, y)
  RegressionQualityMetric.weightedRmse(x, y, .fixInverseAbsoluteWeights(sqrt(abs(y))))
