#' @include weightedRmse.R

#' @title Create an Unweighted Root-Mean Square Error Quality
#'   RegressionQualityMetric
#'
#' @description Create a model quality metric based on the \emph{unweighted}
#' root mean square error. This method is equivalent to calling
#' \code{\link{RegressionQualityMetric.weightedRmse}} with \code{weights} set
#' to \code{NULL}.
#'
#' @param x the input coordinates
#' @param y the output coordinates
#' @return a \code{\link{RegressionQualityMetric}} based on the weighted RMSE
#' @export RegressionQualityMetric.rmse
#' @seealso RegressionQualityMetric.new
#' @seealso RegressionQualityMetric.weightedRmse
RegressionQualityMetric.rmse <- function(x, y) RegressionQualityMetric.weightedRmse(x, y, NULL)
