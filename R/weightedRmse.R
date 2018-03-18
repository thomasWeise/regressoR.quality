#' @include RegressionQualityMetric.R

#' @title Create a Weightd Root-Mean Square Error Quality RegressionQualityMetric with Given
#'   Weights
#'
#' @description Create a model quality metric based on the \emph{weighted} root
#'   mean square error.
#'
#'   Weights can be provided via the parameter \code{weights} and are multiplied
#'   to the errors before the squaring. If no weights are provided (or
#'   \code{weights==NULL}), an un-weighted RMSE measure will be created,
#'   exactly as by \code{\link{rmse}}.
#'
#' @param x the input coordinates
#' @param y the output coordinates
#' @param weights \code{NULL} by default, in which case an un-weighted RMSE will
#'   be created
#' @return a \code{\link{RegressionQualityMetric}} based on the weighted RMSE
#' @export weightedRmse
#' @seealso RegressionQualityMetric.new
#' @seealso rmse
weightedRmse <- function(x, y, weights=NULL) {
   weights <- base::force(weights);
   if(base::is.null(weights)) {
     result <- RegressionQualityMetric.new(
       quality = function(f, ...) {
         z <- (y - f(x, ...));
         base::sqrt(base::mean( z*z ) ) },
       residuals = function(f, ...) (y - f(x, ...)),
       jacobian = function(gradient, ...)
         -base::c(base::do.call(base::rbind, base::lapply(X=x, FUN=gradient))),
       x = x,
       y = y,
       weights = weights
     );
   } else {
     result <- RegressionQualityMetric.new(
                  quality = function(f, ...) {
                    z <- ( (y - f(x, ...)) * weights);
                    base::sqrt( base::mean( z*z ) ) },
                  residuals = function(f, ...) ((y - f(x, ...)) * weights),
                  jacobian = function(gradient, ...)
                    -weights * base::c(base::do.call(base::rbind,
                                       base::lapply(X=x, FUN=gradient))),
                  x = x,
                  y = y,
                  weights = weights
    );
   }
  result <- base::force(result);
  result@quality <- base::force(result@quality);
  result@residuals <- base::force(result@residuals);
  result@jacobian <- base::force(result@jacobian);
  result@x <- base::force(result@x);
  result@y <- base::force(result@y);
  result@weights <- force(result@weights);
  result <- force(result);
  return(result);
}
