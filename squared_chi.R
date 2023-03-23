squared_chi <- function(x, DF, cul = FALSE) {
  #Squared-chi distribution value of x a DF degree of freedom.
  source("gamma_function.R")
  if (cul == FALSE) {
    y <- 1 / (2**(DF / 2)) / gamma(DF / 2) * x**(DF / 2 - 1) * exp(-x / 2)
  }
  if (cul == TRUE) {
    y <- regularized_gamma(x / 2, DF / 2)
  }
  return(y)
}