dist_gamma <- function(x, alpha, beta, cul = FALSE) {
  #Gamma distribution of x. Where alpha is the shape parameter and beta the rate parameter.
  #if it not works, tries the inverse of beta
  source('gamma_function.R')
  if (cul == FALSE) {
    y <- (beta**alpha) * (x**(alpha - 1)) * exp(-beta * x) / gamma(alpha)
  }
  if (cul == TRUE) {
    y <- incomplete_gamma(beta*x, alpha)/gamma(alpha)
  }
  return(y)
}
