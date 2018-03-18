#' @include weightedRmse.R

#' @title Create an Unweighted Root-Mean Square Error Quality RegressionQualityMetric
#'
#' @description Create a model quality metric based on the \emph{unweighted}
#'   root mean square error. This method is equivalent to calling
#'   \code{\link{weightedRmse}} with \code{weights} set to
#'   \code{NULL}.
#'
#' @param x the input coordinates
#' @param y the output coordinates
#' @return a \code{\link{RegressionQualityMetric}} based on the weighted RMSE
#' @export rmse
#' @seealso new
#' @seealso weightedRmse
rmse <- function(x, y) weightedRmse(x, y, NULL)
