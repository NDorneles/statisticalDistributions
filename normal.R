normal <- function(x, mu = 1, sigma = 0, cul = FALSE) {
  #Normal distribuction of x. mu stands for mean, sigma stands for standard deviation and cul stands for the cumulative function.
  #Default values: mu equals 1, sigma equals = 0 and cul equals FALSE
  if (cul == FALSE) {
    y <- 1 / sigma / sqrt(2 * pi) * exp(-0.5 * ((x - mu) / sigma)**2)
  }
  if (cul == TRUE){
    source('erf.R')
    y <- 1 / 2 * (1 + erf((x - mu) / sigma / sqrt(2)))
  }
  return(list(x = x, y = y))
}