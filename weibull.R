weibull <- function(x, k, lambda, cul = FALSE) {
  #Weibull distribution of x for x values higher than 0.
  #k is the shape parameter, lambda is the scale parameter and cul is a boolean value that indicates the computation of the cumulative distribution function.
  if (cul == FALSE) {
    y <- k / lambda * (x / lambda)**(k - 1) * exp(-((x / lambda)**k))
  }
  if (cul == TRUE) {
    y <- 1 - exp(-((x / lambda)**k))
  }
  return(y)
}
