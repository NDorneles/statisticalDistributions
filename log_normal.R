log_normal <- function(x, mu = 0, sigma = 1, cul = FALSE) {
  #Log-Normal distribuction of x. mu stands for mean, sigma stands for standard deviation and cul stands for the cumulative function.
  #Default values: mu equals 1, sigma equals = 0 and cul equals FALSE
  if (cul == FALSE) {
    y <- 1 / sigma / x / sqrt(2 * pi) * exp(-0.5 * ((log(x) - mu) / sigma)**2)
  }
  if (cul == TRUE) {
    source('erf.R')
    y <- 1 / 2 * (1 + erf((log(x) - mu) / sigma / sqrt(2)))
  }
  return(y)
}

log_normal(0.7)
