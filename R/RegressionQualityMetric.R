#' @title A Quality RegressionQualityMetric for Fittable Models
#' @description A quality metric allows us to judge how well a model fits to
#'   some \code{x-y} data.
#'
#'   A quality metric is always created based on a specific dataset consisting
#'   of known input values \code{x} and expected output values \code{y} of an
#'   unknown function. A quality metric is used to judge how well another
#'   function \code{f} accepting a single parameter \code{x} (and potentially a
#'   set of other parameters, e.g., a parameterization \code{par}) fits to this
#'   data.
#'
#'   It therefore has at least one function \code{quality(f, ...)} which is the
#'   direct quality measure for \code{f(x, ...)}. For each element of the vector
#'   \code{x}, \code{quality} computes the result of \code{f(x, ...)} of the
#'   function and check how far away it is from the expected corresponding value
#'   in \code{y}. It will then return a positive value which is the bigger the
#'   worse \code{f} fits to the data and \code{0} in case of a perfect fit.
#'
#'   A model quality measure may have two more optional (potentially
#'   \code{NULL}) components: \code{residuals(f, ...)} is a function which
#'   returns a vector of residuals, i.e., some potentially weighted values of
#'   the form \code{y - f(x,...)} which represent, for each value in \code{x},
#'   the corresponding deviation from \code{y}. \code{jacobian(gradient, ...)}
#'   returns the Jacobian matrix based on a user-provided \code{gradient}
#'   function in the format that \code{nls.lm} expects. If both components are
#'   not \code{NULL}, they can be used for performing a nonlinear least squares
#'   approximation of the correct parameterization of \code{f}. If they are
#'   \code{NULL}, the quality metric may only be useful for some black-box
#'   optimization approach to parameterization or to judge a model directly
#'   derived from the data via, e.g., kriging.
#'
#' @slot quality the quality function of the type \code{quality(f, ...)},
#'   passing any additional parameter directly to \code{f}, and returning the
#'   scalar raw quality metric, as needed when performing numerical optimization
#' @slot x the vector of input values, or \code{NULL} if not specified
#' @slot y the vector of output values, or \code{NULL} if not specified
#' @slot residuals the residuals function of the type \code{residuals(f, ...)},
#'   passing any additional parameter directly to \code{f}, and returning the
#'   (potentially weighted) residuals vector, as needed by \code{nls.lm}-like
#'   optimizations, or \code{NULL} if unspecified
#' @slot jacobian a jacobian function of the type \code{jacobian(gradient,
#'   ...)}, passing any additional parameter directly to \code{gradient}, and
#'   returning the Jacobian based on the gradient vectors, as used by
#'   \code{nls.lm}-like optimizations, or \code{NULL} if unspecified.
#' @slot weights  the vector of weights, or \code{NULL} if not specified (in
#'   which case, all weights will be \code{1})
#' @seealso new
#' @importFrom methods setClass representation prototype
#' @importClassesFrom utilizeR functionOrNULL numericOrNULL
#' @importFrom utilizeR function.args
#' @exportClass RegressionQualityMetric
RegressionQualityMetric <- setClass(
  Class = "RegressionQualityMetric",
  representation = representation(quality="function",
                                  x="numeric",
                                  y="numeric",
                                  residuals="functionOrNULL",
                                  jacobian="functionOrNULL",
                                  weights="numericOrNULL"),
  prototype(residuals=NULL, jacobian=NULL, weights=NULL),
  validity = function(object) {
    # Check the quality metric
    if(is.null(object@quality) || (!(is.function(object@quality)))) {
      return("Quality function must be properly defined.");
    }
    # Check the arguments of the quality function
    if (!(identical(function.args(object@quality), c("f", "...")))) {
      return("The quality function must take exactly two arguments named 'f' and '...'.");
    }

    # Check residuals and jacobian function
    if(is.null(object@residuals)){
      if (!(is.null(object@jacobian))) {
        return("Jacobian function cannot be defined if residuals function is not defined.");
      }
    } else {
      # Check whether the residuals function is a proper function
      if(!(is.function(object@residuals))) {
        return("If not null, then the residuals function must be properly defined.");
      }
      # Check the arguments of the quality function
      if (!(identical(function.args(object@residuals), c("f", "...")))) {
        return("If specified, the residuals function must take exactly two arguments two arguments named 'f' and '...'.");
      }

      if(!(is.null(object@jacobian))) {

        # Check whether the jacobian function is a proper function
        if(!(is.function(object@jacobian))) {
          return("If not null, then the jacobian function must be properly defined.");
        }
        # Check the arguments of the quality function
        if (!(identical(function.args(object@jacobian), c("gradient", "...")))) {
          return("If specified, the jacobian function must take exactly two arguments two arguments named 'gradient' and '...'.");
        }
      }
    }

    # Check x and y vectors
    if(is.null(object@x) || (!(is.vector(object@x)))){
      return("x must be a non-null vector.");
    }
    len <- length(object@x);
    if(len <= 0L) {
      return("Length of x vector cannot be 0.");
    }

    if(is.null(object@y) || (!(is.vector(object@y)))){
      return("y must be a non-null vector.");
    }
    if(len != length(object@y)) {
      return("Length of x and y vector must be the same.");
    }

    if(!(is.null(object@weights))) {
      if((!(is.vector(object@weights))) ||
         (length(object@weights) != len)) {
        return("If weights vector is specified, it must have the same length as the x and y vector.");
      }
    }

    return(TRUE);
  }
)


#' @title Create a new instance of \code{\link{RegressionQualityMetric}}
#' @description Instantiate the class \code{\link{RegressionQualityMetric}}
#' @param quality the quality function
#' @param x the vector of input values
#' @param y the vector if output values
#' @param residuals the residuals function (\code{NULL} if undefined)
#' @param jacobian the jacobian function (\code{NULL} if undefined)
#' @param weights the weights vector (\code{NULL} if undefined)
#' @return the new model quality metric
#' @export RegressionQualityMetric.new
#' @importFrom methods new validObject
RegressionQualityMetric.new <- function(quality, x, y, residuals=NULL, jacobian=NULL, weights=NULL) {
  result <- new("RegressionQualityMetric",
                         quality=quality,
                         x=x,
                         y=y,
                         residuals=residuals,
                         jacobian=jacobian,
                         weights=weights);
  result <- force(result);
  result@quality <- force(result@quality);
  result@x <- force(result@x);
  result@y <- force(result@y);
  result@residuals <- force(result@residuals);
  result@jacobian <- force(result@jacobian);
  result@weights <- force(result@weights);
  result <- force(result);
  validObject(result);
  return(result);
}
