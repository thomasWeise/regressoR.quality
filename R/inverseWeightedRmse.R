#' @include weightedRmse.R
#' @include fixInverseAbsoluteWeights.R

#' @title Create a Root-Mean Square Error Quality RegressionQualityMetric where the Weights are the Inverse Absolute \code{y} Values
#'
#' @description Create a model quality metric based on the root mean square
#'   error, using the inverse absolute \code{y} values as weights.
#'
#' The idea behind this method is to provide a weighted quality measure which
#' can handle both very large and very small values of \code{y}. Say you want to
#' fit a model to a dataset where one point has \code{y1=1e20} and another one
#' has \code{y2=1e-10}. In this case, if the model predicts \code{y1'=1e19} for
#' the former point, i.e., has an absolute error of \code{0.9e20}, it almost
#' does not matter which value it predicts for the second point. Even if it
#' would predict \code{y2'=1e8}, which would be off by almost \code{1e16}
#' percent, this error will not really matter in absolute comparison. However,
#' if we weight the points by a reasonable function of the scale of their
#' \code{y} values, than all errors play a reasonable role: in the first case,
#' dividing \code{0.9e20} by \code{1e20} would yield \code{0.9}. In the second
#' case, the absolute error would be roughly \code{|(1e-10-1e8)/1e-10|=1e18}, so
#' the error becomes relevant. If we would predict \code{1e-11} for the second
#' point, then this would yield an absolute error of \code{0.9} as well.
#'
#' Of course, special care needs to be taken with \code{y=0}. In this case, a
#' reasonable default weight is computed.
#'
#' @param x the input coordinates
#' @param y the output coordinates
#' @return a \code{\link{RegressionQualityMetric}} based on the RMSE weighted by the
#'   inverse absolute \code{y} values.
#' @export inverseWeightedRmse
#' @seealso new
#' @seealso weightedRmse
inverseWeightedRmse <- function(x, y)
  weightedRmse(x, y, .fixInverseAbsoluteWeights(base::abs(y)))
