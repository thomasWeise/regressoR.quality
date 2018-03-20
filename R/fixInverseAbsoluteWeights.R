# @title An internal helper method to fix weights
# @description This method translates inverse weights to weights by taking care
#   of zero values
# @param inverseAbsoluteWeights the inverse weights
# @return the actual weights, or \code{NULL} if no actual weights can or need to
#   be computed
.fixInverseAbsoluteWeights <- function(inverseAbsoluteWeights=NULL) {
  max1 <- -Inf;
  max2 <- -Inf;
  min1 <- +Inf;

  # if there is only a single value, it is not necessary to weight it
  if(base::length(inverseAbsoluteWeights) <= 1L) {
    return(NULL);
  }

  # find the two biggest and 1 smallest inverse weights
  for(weight in inverseAbsoluteWeights) {
    if(weight > max1) {
      max2 <- max1;
      max1 <- weight;
    } else {
      if(weight > max2) {
        max2 <- weight;
      }
    }
    if(weight < min1) {
      min1 <- weight;
    }
  }

  if((min1 > 0) && base::is.finite(min1)) {
    # ok, the smallest absolute output value is bigger than 0,
    # so we do not need any 0-handling
    return(1 / inverseAbsoluteWeights);
  }
  if((max1 <= 0) || (!(base::is.finite(max1)))) {
    # there is no finite non-zero value, so we do not need to use weights
    return(NULL);
  }
  # expect some in-finite weights, which we then have to fix
  weights <- (1 / inverseAbsoluteWeights);
  zeroWeight <- 0;
  if((max1 > max2) && (max2 > 0) && base::is.finite(max2)) {
    # if we have two distinct weights larger than 0
    # we can extend their relationship as weight for the smallest point
    zeroWeight <- (max2 / (max1 * max1))
  }

  # Check if it was possible to extend the weight relationship
  if(zeroWeight <= 0) {
    # ok, no two distinct non-zero values
    zeroWeight <- 0.8/max1;
    if(zeroWeight <= 0) {
      zeroWeight <- 1/max1;
      if(zeroWeight <= 0) {
        # let's take the smallest finite weight then
        zeroWeight = base::min(weights[base::is.finite(weights)]);
        if(zeroWeight <= 0) {
          # no dice, we can use the default rmse
          return(NULL);
        }
      }
    }
  }

  # replace all in-finite weights with the weight for the zero value
  weights[!base::is.finite(weights)] <- zeroWeight;
  return(weights);
}
