#' @include sqrtInverseWeightedRmse.R

#' @title Get the Default Model Quality RegressionQualityMetric
#'
#' @description This method returns a default model quality metric for the given
#'   coordinates \code{x} and \code{y}. Currently, this is implemented as
#'   \code{\link{RegressionQualityMetric.sqrtInverseWeightedRmse}}.
#'
#' @param x the input coordinates
#' @param y the output coordinates
#' @return a default instance of \code{\link{RegressionQualityMetric}} which
#'   should work well in many scenarios
#' @export RegressionQualityMetric.default
#' @seealso RegressionQualityMetric.sqrtInverseWeightedRmse
RegressionQualityMetric.default <- RegressionQualityMetric.sqrtInverseWeightedRmse
