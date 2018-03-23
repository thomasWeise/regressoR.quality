#' @include RegressionQualityMetric.R

#' @title Create a Weightd Root-Mean Square Error Quality
#'   RegressionQualityMetric with Given Weights
#'
#' @description Create a model quality metric based on the \emph{weighted} root
#' mean square error.
#'
#' Weights can be provided via the parameter \code{weights} and are multiplied
#' to the errors before the squaring. If no weights are provided (or
#' \code{weights==NULL}), an un-weighted RMSE measure will be created, exactly
#' as by \code{\link{RegressionQualityMetric.rmse}}.
#'
#' @param x the input coordinates
#' @param y the output coordinates
#' @param weights \code{NULL} by default, in which case an un-weighted RMSE will
#'   be created
#' @return a \code{\link{RegressionQualityMetric}} based on the weighted RMSE
#' @export RegressionQualityMetric.weightedRmse
#' @seealso RegressionQualityMetric.new
#' @seealso RegressionQualityMetric.rmse
RegressionQualityMetric.weightedRmse <- function(x, y, weights=NULL) {
   weights <- force(weights);
   if(is.null(weights) || (length(weights) <= 1L)) {
     result <- RegressionQualityMetric.new(
       quality = function(f, ...) {
         z <- (y - f(x, ...));
         sqrt(mean( z*z ) ) },
       residuals = function(f, ...) (y - f(x, ...)),
       jacobian = function(gradient, ...)
         -c(do.call(rbind, lapply(X=x, FUN=gradient))),
       x = x,
       y = y,
       weights = weights
     );
   } else {
     result <- RegressionQualityMetric.new(
                  quality = function(f, ...) {
                    z <- ( (y - f(x, ...)) * weights);
                    sqrt( mean( z*z ) ) },
                  residuals = function(f, ...) ((y - f(x, ...)) * weights),
                  jacobian = function(gradient, ...)
                    -weights * c(do.call(rbind, lapply(X=x, FUN=gradient))),
                  x = x,
                  y = y,
                  weights = weights
    );
   }
  result <- force(result);
  result@quality <- force(result@quality);
  result@residuals <- force(result@residuals);
  result@jacobian <- force(result@jacobian);
  result@x <- force(result@x);
  result@y <- force(result@y);
  result@weights <- force(result@weights);
  result <- force(result);
  return(result);
}
