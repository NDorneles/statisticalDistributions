student <- function(x, DF, cul = FALSE) {
  #t-student distribution value of x with DF as a degree of freedom.
  if (cul == FALSE) {
    source("gamma_function.R")
    y <- gamma((DF + 1) / 2) / sqrt(DF * pi) / gamma(DF / 2)*(1 + x**2 / DF)**(-(DF + 1) / 2)
  }
  if (cul == TRUE) {
    source("beta_function.R")
    t <- DF / (x**2 + DF)
    y <- regularized_beta(t,DF / 2, 0.5)
    y <- 1 - 0.5 * y
    if(x < 0) {
      y <- 1 - y
    }
  }
  return(y)
}