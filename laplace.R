laplace <- function(x, mu, beta, cul = FALSE) {
  #Laplace distribution of x. Where mu is the location parameter, beta is the scale parameter and cum indicates the computation of the cumulative function. 
  if (cul == FALSE) {
    y <- 1 / 2 / beta * exp(-abs(x - mu) / beta)
  }
  if (cul == TRUE) {
    y <- 1 / 2 + 1 / 2 * sign(x - mu) * (1 - exp(-abs(x - mu) / beta))
  }
  return(y)
}
